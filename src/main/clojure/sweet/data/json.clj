(ns ^:skip-aot? sweet.data.json
  (:refer-clojure :exclude [read])
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayOutputStream]))


(declare read write read-str write-str read-bytes write-bytes mapper)


(cond-require
 'jsonista.core
 (load "json/jsonista")

 'cheshire.core
 (load "json/cheshire_core")

 'clojure.data.json
 (load "json/clojure_data_json")

 :else
 (throw "cheshire.core OR clojure.data.json required"))


(when-not (bound? #'mapper)
  (def mapper identity))

(when-not (bound? #'read-bytes)
  (defn read-bytes
    ([^bytes bs]
     (read (io/reader bs)))
    ([^bytes bs mapper]
     (read (io/reader bs) mapper))))

(when-not (bound? #'write-bytes)
  (defn write-bytes
    (^bytes [x]
     (let [out (ByteArrayOutputStream.)]
       (write x (io/writer out))
       (.toByteArray out)))
    (^bytes [x mapper]
     (let [out (ByteArrayOutputStream.)]
       (write x (io/writer out) mapper)
       (.toByteArray out)))))
