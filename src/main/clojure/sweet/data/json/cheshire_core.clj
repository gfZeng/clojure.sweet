(in-ns 'sweet.data.json)


(require '[cheshire.core :refer [parse-string
                                 parse-stream
                                 generate-string
                                 generate-stream]]
         '[cheshire.parse :refer (*use-bigdecimals?*)])

(defn read [rdr {:keys [encode-key-fn value-fn bigdecimals]}]
  (binding [*use-bigdecimals?* bigdecimals]
    (parse-stream rdr key-fn value-fn)))

(defn read-str [rdr {:keys [encode-key-fn value-fn bigdecimals]}]
  (binding [*use-bigdecimals?* bigdecimals]
    (parse-string rdr key-fn value-fn)))

(defn write [x wrt mapper]
  (generate-stream x wrt mapper))

(defn write-str [x mapper]
  (generate-string x mapper))


