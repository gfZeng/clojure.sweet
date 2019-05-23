(ns ^:skip-aot? sweet.data.transit
  (:refer-clojure :exclude [read])
  (:require [cognitect.transit :as transit])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))



(defalias cognitect.transit)


(defn write-bytes
  ([x] (write-bytes x {}))
  ([x opts]
   (with-open [out (ByteArrayOutputStream. 4096)]
     (let [wrt (transit/writer out :json opts)]
       (write wrt x)
       (.toByteArray out)))))

(defn write-str
  ([x]      (write-str x {}))
  ([x opts] (String. (write-bytes x opts))))

(defn read-bytes
  ([bs] (read-bytes bs {}))
  ([bs opts]
   (with-open [in (ByteArrayInputStream. bs)]
     (let [rdr (transit/reader in :json opts)]
       (read rdr)))))

(defn read-str
  ([s]      (read-str s {}))
  ([s opts] (read-bytes (.getBytes s) opts)))


