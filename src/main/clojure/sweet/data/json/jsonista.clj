(in-ns 'sweet.data.json)


(require '[jsonista.core :as jsonista])


(defalias read        jsonista/read-value)
(defalias read-str    jsonista/read-value)
(defalias read-bytes  jsonista/read-value)

(defalias write       jsonista/write-value)
(defalias write-str   jsonista/write-value-as-string)
(defalias write-bytes jsonista/write-value-as-bytes)

(defn mapper [opts]
  (jsonista/object-mapper opts))
