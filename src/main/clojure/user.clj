(ns user
  (:refer-clojure :exclude [defalias if-require when-require cond-require
                            extend-with-coerce])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :refer (info)]

            [sweet.reader])
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
   `(do
      (require '~ns)
      ~@(for [sym# (keys (ns-publics ns))
              :let [orig# (symbol (name ns) (name sym#))]]
          (list 'defalias sym# orig#))))
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


(defmulti model-mixin (fn [mixin name fields] mixin))

(defmethod model-mixin :lookup [_ name fields]
  `(clojure.lang.ILookup
    ~'(valAt [this k]
             (.valAt this k nil))
    (~'valAt ~'[this k not-found]
     (case ~'k
       ~@(interleave (map keyword fields) fields)
       ~'not-found))))

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
        mixin  (disj mixin :record :type :protocol)]
    `(do
       ~pdef
       (~thedef ~model ~fields
        ~@pimplsdef
        ~@more
        ~@(mapcat #(model-mixin % model fields) mixin)))))


(defalias clojure.core/defalias defalias)
(defalias clojure.core/if-require if-require)
(defalias clojure.core/when-require when-require)
(defalias clojure.core/cond-require cond-require)
(defalias clojure.core/extend-with-coerce extend-with-coerce)
(defalias clojure.core/defmodel defmodel)
(defalias clojure.core/model-mixin model-mixin)


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
