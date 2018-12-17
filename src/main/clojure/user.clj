(ns user
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.channels
            WritableByteChannel
            ReadableByteChannel
            Channels]))


(when-some [conf (io/resource "logging.properties")]
  (.. java.util.logging.LogManager
      getLogManager
      (readConfiguration (io/input-stream conf))))

(require '[clojure.tools.logging :refer (info spyf)])


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

(def COERCIONS
  {'int   #(Integer/parseInt %)
   'long  #(Long/parseLong %)
   'ints  (fn [s] (mapv #(Integer/parseInt %)
                        (str/split s #",")))
   'longs (fn [s] (mapv #(Long/parseLong %)
                        (str/split s #",")))})

(defn read-property
  ([x] (if (sequential? x)
         (read-property (first x) (second x))
         (read-property x clojure.lang.Var$Unbound)))
  ([x not-found]
   (let [tag  (:tag (meta x))
         k    (str x)
         envk (str/upper-case (str/replace k #"[-.]" "_"))
         v    (or (System/getProperty k)
                  (System/getenv envk))]
     (if (nil? v)
       (spyf :info (str "#prop " x " default: %s")
             not-found)
       (spyf :info (str "#prop " x "  config: %s")
             ((COERCIONS tag identity) v))))))

(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
  any) and similar metadata. The metadata of the alias is its initial
  metadata (as provided by def) merged into the metadata of the original."
  ([ns]
   `(do
      (require ~ns)
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


(defalias clojure.core/defalias defalias)
(defalias clojure.core/if-require if-require)
(defalias clojure.core/when-require when-require)
(defalias clojure.core/cond-require cond-require)

(when-require 'clojure.core.async
  (load "core_async"))
