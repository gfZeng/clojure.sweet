(ns user
  (:refer-clojure :exclude [defalias
                            extend-with-coerce
                            defmodel model-mixin
                            if-require when-require cond-require
                            return-as
                            Mappable
                            ->map])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.tools.logging :refer (info)]

            [sweet.reader]
            [medley.core])
  (:import [java.util Properties]
           [java.io PipedOutputStream PipedInputStream]
           [java.nio.channels
            WritableByteChannel
            ReadableByteChannel
            Channels]))

(extend-protocol io/IOFactory
  WritableByteChannel
  (make-output-stream [x _]
    (Channels/newOutputStream x))

  ReadableByteChannel
  (make-input-stream [x _]
    (Channels/newInputStream x)))


(alter-var-root
 #'*assert*
 (constantly (.desiredAssertionStatus clojure.main)))

(info `*assert* *assert*)

(defn set-default-exception-handler [h]
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (^void uncaughtException [this ^Thread t ^Throwable e]
      (h t e)))))

(defmacro extend-with-coerce [protocol type coerce]
  (let [p    @(resolve protocol)
        sigs (vals (:sigs p))
        ns   (-> (:method-builders p) first key (.-ns) (.-name))

        coerce-sym (gensym "coerce_")
        sig-form   (fn [f arglists]
                     (let [nsf (symbol (name ns) (name f))]
                       (cons f (for [args arglists]
                                 `(~args (~nsf (~coerce-sym ~(first args)) ~@(rest args)))))))]
    `(let [~coerce-sym ~coerce]
       (extend-type ~type
         ~protocol
         ~@(for [{f :name arglists :arglists} sigs]
             (sig-form f arglists))))))

(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([ns]
   (require ns)
   (when-not (:ns &env) ; temperary disable in clojurescript
     `(do
        (require '~ns)
        ~@(for [sym# (keys (ns-publics ns))
                :let [orig# (symbol (name ns) (name sym#))]]
            (list 'defalias sym# orig#)))))
  ([dst orig]
   `(let [sv#   (resolve '~orig)
          ns#   '~(symbol (or (namespace dst) (ns-name *ns*)))
          name# '~(symbol (name dst))]
      (doto (intern ns# name# (.getRawRoot sv#))
        (alter-meta! merge (meta sv#)))))
  ([dst orig doc]
   (list `defalias (with-meta dst (assoc (meta name) :doc doc)) orig)))

(defmacro if-require
  ([ns then] `(if-require ~ns ~then nil))
  ([ns then else]
   `(if (try (require ~ns) true (catch Throwable ~'_ false))
      ~then
      ~else)))

(defmacro when-require [ns & body]
  `(if-require ~ns (do ~@body)))

(defmacro cond-require
  ([] nil)
  ([ns then & elses]
   (if (= ns :else)
     then
     `(if-require ~ns ~then (cond-require ~@elses)))))

(defn name-with-meta
  "Given a symbol and args, returns [<name-with-meta-meta> <args>] with
  support for `defn` style `?doc` and `?attrs-map`."
  ([sym args            ] (name-with-meta sym args nil))
  ([sym args attrs-merge]
   (let [[?doc args]  (if (and (string? (first args)) (next args))
                        [(first args) (next args)]
                        [nil args])
         [attrs args] (if (and (map?    (first args)) (next args))
                        [(first args) (next args)]
                        [{}  args])
         attrs        (if ?doc (assoc attrs :doc ?doc) attrs)
         attrs        (if (meta sym) (conj (meta sym) attrs) attrs)
         attrs        (conj attrs attrs-merge)]
     [(with-meta sym attrs) args])))


(defprotocol Mappable
  (->map [this]))

(defmulti model-mixin (fn [mixin name fields] mixin))

(defmethod model-mixin :lookup [_ name fields]
  (let [keys (map keyword fields)]
    {:dynamics `(clojure.lang.ILookup
                 ~'(valAt [this k]
                          (.valAt this k nil))
                 (~'valAt ~'[this k not-found]
                  (case ~'k
                    ~@(interleave keys fields)
                    ~'not-found)))}))

(defmethod model-mixin :map [_ name fields]
  (let [mksym  (symbol (str "make-" name))
        entmap (symbol (str name "-map"))
        build  (if (resolve entmap)
                 `(~entmap ~'m)
                 'm)
        ofkeys (map keyword fields)
        keys   (->> fields
                    (remove #(str/starts-with? % "_"))
                    (mapv keyword))]
    {:statics  `((defn ~mksym [~'m]
                   (let [~'m ~build]
                     (new ~name ~@(map #(list % 'm) ofkeys)))))
     :dynamics `(Mappable
                 (~'->map [~'this]
                  (array-map ~@(interleave keys (map #(list % 'this) keys)))))}))

(defn- symbol-argv [argv]
  (mapv #(if (symbol? %) % (gensym "arg_")) argv))

(defmacro defmodel
  {:style/indent '(2 nil nil (:defn))}
  [model fields & impls]
  (let [[mixin impls] (split-with keyword? impls)
        [pimpls more] (split-with seq? impls)

        pimpls    (group-by first pimpls)
        sigs      (for [[fname impls] pimpls]
                    (cons fname (map (comp symbol-argv second) impls)))
        protocol? (boolean (seq pimpls))
        pname     (symbol (str \I model))
        pdef      (when protocol?
                    `(defprotocol ~pname
                       ~@sigs))
        pimplsdef (when protocol?
                    `(~pname ~@(mapcat val pimpls)))

        mixin  (set mixin)
        thedef (if (:record mixin)
                 `defrecord
                 `deftype)
        mixin  (disj mixin :record :type :protocol)
        exts   (->> (model-mixin % model fields)
                    (for [% mixin])
                    (apply merge-with concat))]
    `(do
       ~pdef
       (~thedef ~model ~fields
        ~@pimplsdef
        ~@more
        ~@(:dynamics exts))
       ~@(:statics exts)
       ~model)))

(defmacro return-as [x & body]
  (let [body (walk/postwalk-replace {x `(deref ~x)} (cons 'do body))]
    `(let [~x   (volatile! nil)
           ret# ~body]
       (vreset! ~x ret#)
       ret#)))

(defn xor
  ([x y]        (and (or x y)
                     (not (and x y))))
  ([x y & more] (apply xor (xor x y) more)))

(defalias clojure.core/defalias defalias)
(in-ns 'clojure.core)
(defalias medley.core)
(in-ns 'user)
(refer-clojure)


(defalias clojure.core/set-default-exception-handler set-default-exception-handler)
(defalias clojure.core/if-require if-require)
(defalias clojure.core/when-require when-require)
(defalias clojure.core/cond-require cond-require)
(defalias clojure.core/extend-with-coerce extend-with-coerce)
(defalias clojure.core/defmodel defmodel)
(defalias clojure.core/model-mixin model-mixin)
(defalias clojure.core/return-as return-as)
(defalias clojure.core/Mappable Mappable)
(defalias clojure.core/->map ->map)
(defalias clojure.core/xor xor)


(def ^:private AFTER-LOADS (atom {}))

(defonce alter-load-one
  (delay
   (alter-var-root
    #'clojure.core/load-one
    (fn [f]
      (fn [lib need-ns require]
        (let [ret (f lib need-ns require)]
          (when-some [f (@AFTER-LOADS lib)]
            (f))
          ret))))))

(defn after-load [ns f]
  @alter-load-one
  (swap! AFTER-LOADS assoc ns f))



(after-load 'clojure.core.async #(load "/core_async"))

(when (io/resource "user.ext.clj")
  (load "user.ext"))
