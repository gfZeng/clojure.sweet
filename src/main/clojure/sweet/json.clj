(ns sweet.json
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io]))


(declare read write read-str write-str read-bytes write-bytes)


(cond-require
 'jsonista.core
 (load "jsonista_core")

 'cheshire.core
 (load "cheshire_core")

 'clojure.data.json
 (load "clojure_data_json")

 :else
 (throw "cheshire.core OR clojure.data.json required"))


(when-not (bound? #'read-bytes)
  (defn read-bytes [^bytes bs & opts]
    (apply read (io/reader bs) opts)))

(when-not (bound? #'write-bytes)
  (defn write-bytes ^bytes [x & opts]
    (let [out (ByteArrayOutputStream.)]
      (apply write x (io/writer out) opts)
      (.toByteArray out))))
