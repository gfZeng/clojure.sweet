(in-ns 'sweet.json)


(require '[cheshire.core :refer [parse-string
                                 parse-stream
                                 generate-string
                                 generate-stream]])

(defn read [rdr & {:keys [biddec key-fn value-fn]}]
  (binding [cheshire.parse/*use-bigdecimals?* bigdec]
    (parse-stream rdr key-fn value-fn)))

(defn read-str [rdr & {:keys [biddec key-fn value-fn]}]
  (binding [cheshire.parse/*use-bigdecimals?* bigdec]
    (parse-string rdr key-fn value-fn)))

(defn write [x wrt & {:as opts}]
  (generate-stream x wrt opts))

(defn write-str [x wrt & {:as opts}]
  (generate-string x opts))


