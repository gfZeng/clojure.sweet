(in-ns 'sweet.json)


(require '[cheshire.core :refer [parse-string
                                 parse-stream
                                 generate-string
                                 generate-stream]]
         '[cheshire.parse :refer (*use-bigdecimals?*)])

(defn read [rdr & {:keys [bigdec key-fn value-fn]
                   :or   {bigdec *use-bigdecimals?*}}]
  (binding [*use-bigdecimals?* bigdec]
    (parse-stream rdr key-fn value-fn)))

(defn read-str [rdr & {:keys [bigdec key-fn value-fn]
                       :or   {bigdec *use-bigdecimals?*}}]
  (binding [*use-bigdecimals?* bigdec]
    (parse-string rdr key-fn value-fn)))

(defn write [x wrt & {:as opts}]
  (generate-stream x wrt opts))

(defn write-str [x & {:as opts}]
  (generate-string x opts))


