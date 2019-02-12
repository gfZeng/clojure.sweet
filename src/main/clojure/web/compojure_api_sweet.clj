(when-not (find-ns 'compojure.api.sweet)
  (throw (ex-info "compojure.api.sweet required" {})))
(in-ns 'compojure.api.sweet)
(require '[clojure.tools.logging :refer (warn debug)]
         '[clojure.string :as str])

(defalias ring.util.http-response)
(defalias route-middleware compojure.api.core/route-middleware)

(defn endpoint? [h]
  (if (var? h)
    (endpoint? @h)
    (satisfies? compojure.api.routes/Routing h)))

(defmacro defendpoint [name method path & body]
  (let [[name method path body]
        (if (symbol? method)
          [name (ns-resolve 'compojure.api.sweet method)
           path body]
          [(-> (str name "_" (clojure.string/replace method #"/" "_"))
               (symbol)
               (with-meta (meta name)))
           (ns-resolve 'compojure.api.sweet name)
           method
           (cons path body)])

        name         (vary-meta name assoc
                                ::endpoint? true)
        [req & body] body]
    `(def ~name
       ~(apply method &form &env path req
               :tags (or (:swagger/tags (meta *ns*))
                         [(-> (str (ns-name *ns*))
                              (str/split #"\.")
                              (last))])
               body))))

(defn unmap-endpoints [ns]
  (-> (the-ns ns)
      (ns-publics)
      (run! (fn [[sym var]]
              (when (endpoint? var)
                (ns-unmap ns sym))))))

(defn ns-routes
  ([ns]
   (ns-routes ns nil))
  ([ns reload?]
   (when (and (symbol? ns) (nil? (find-ns ns)))
     (require ns))
   (->> (the-ns ns)
        (ns-publics)
        (vals)
        (filter endpoint?)
        (map (if reload? identity deref))
        (apply routes))))

(defn wrap-timing [h]
  (fn
    ([req]
     (let [ts  (System/nanoTime)
           res (h req)
           end (System/nanoTime)]
       (debug (:request-method req) (:uri req) (/ (- end ts) 1E6))
       res))
    ([req respond raise]
     (let [ts (System/nanoTime)]
       (h req #(let [end (System/nanoTime)]
                 (debug (:request-method req) (:uri req) (/ (- end ts) 1E6))
                 (respond %))
          raise)))))

(defn anomaly
  ([key] (anomaly key nil))
  ([key msg]
   (let [body {:error key :msg msg}]
     (case key
       :anomaly/conflict     (conflict body)
       :anomaly/unauthorized (unauthorized body)
       :else                 (internal-server-error body)))))

(defn json-use-bigdec []
  (if-some [var (resolve 'cheshire.parse/*use-bigdecimals?*)]
    (alter-var-root var (constantly true))
    (warn "cheshire required")))


;; ring.util.http-response
(defn invalid-token
  ([]     (invalid-token nil))
  ([body] {:status  498
           :headers {}
           :body    body}))
