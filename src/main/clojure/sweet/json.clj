(ns sweet.json
  (:refer-clojure :exclude [read]))


(declare read write read-str write-str)


(cond-require
 'cheshire.core
 (load "cheshire_core")

 'clojure.data.json
 (load "clojure_data_json")

 :else
 (throw "cheshire.core OR clojure.data.json required"))
