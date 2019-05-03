(ns ^:skip-aot? sweet.data.transit
  (:refer-clojure :exclude [read])
  (:require [cognitect.transit :as transit])
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]))



(defalias cognitect.transit)


(defn write-bytes [x]
  (with-open [out (ByteArrayOutputStream. 4096)]
    (let [wrt (transit/writer out :json)]
      (write wrt x)
      (.toByteArray out))))

(defn write-str [x]
  (String. (write-bytes x)))

(defn read-bytes [bs]
  (with-open [in (ByteArrayInputStream. bs)]
    (let [rdr (transit/reader in :json)]
      (read rdr))))

(defn read-str [s]
  (read-bytes (.getBytes s)))


