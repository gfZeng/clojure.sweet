(in-ns 'sweet.data.json)

(defalias clojure.data.json)

(defn read [rdr mapper]
  (apply read rdr (apply concat mapper)))

(defn read-str [str mapper]
  (apply read-str str (apply concat mapper)))

(defn write [x wrt mapper]
  (apply write x wrt (apply concat mapper)))

(defn write-str [x mapper]
  (apply write-str x (apply concat mapper)))

