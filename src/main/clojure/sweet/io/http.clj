(ns ^:skip-aot? sweet.io.http
  (:require [clojure.core.async :as a
             :refer (go go-loop <! >! chan timeout <!!)]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [aleph.http :as http]
            [clojure.tools.logging :as log
             :refer (debug info warn error fatal spy)]
            [clojure.string :as str]))


(defn default-exception-handler [e req pch]
  (a/close! pch)
  (error e "uncaught in for request" req))


(defn fetch
  ([req]     (fetch req (a/promise-chan)))
  ([req pch] (fetch req pch default-exception-handler))
  ([req pch exception-handler]
   (-> (http/request req)
       (d/chain' #(a/put! pch %))
       (d/catch' #(exception-handler % req pch))
       (d/catch' #(default-exception-handler % req pch)))
   pch))


(defmacro defrequest [name]
  (let [method (keyword (str/lower-case (str name)))]
    `(defn ~name
       {:arglist (:arglist #'fetch)}
       ([req#] (~name req# (a/promise-chan)))
       ([req# pch#] (~name req# pch# default-exception-handler))
       ([req# pch# exh#] (fetch (assoc req# :method ~method) pch# exh#)))))

(defrequest GET)
(defrequest POST)
(defrequest PUT)
(defrequest PATCH)
(defrequest DELETE)
(defrequest OPTION)
(defrequest HEAD)


(defn websocket-client
  ([req]     (websocket-client req (a/promise-chan)))
  ([req pch] (websocket-client req pch default-exception-handler))
  ([{url :url :as req}  pch exception-handler]
   (-> (http/websocket-client url req)
       (d/chain' #(a/put! pch %))
       (d/catch' #(exception-handler % req pch))
       (d/catch' #(default-exception-handler % req pch)))
   pch))


(defn send!
  ([ws x] (send! ws x (a/promise-chan)))
  ([ws x pch]
   (-> (s/put! ws x)
       (d/chain' #(a/put! pch %)))
   pch))

(defn websocket-connect
  ([ws in]
   (websocket-connect ws in true))
  ([ws in close?]
   (s/connect ws in  {:downstream? close? :upstream? true})))


(defrecord Command [type data])

(defn websocket
  ([req]        (websocket req (a/duplex (a/chan 10) (a/chan))))
  ([req duplex] (websocket req duplex default-exception-handler))
  ([req {:keys [in out] :as duplex} exception-handler]
   (let [reconnect?  (:auto-reconnect? req)
         cmds        (a/pipe out (a/chan 10))
         fire-events (fn [registry event]
                       (doseq [f (vals (registry event))]
                         (f)))]
     (go-loop [ws       nil
               registry {}
               buf      []]
       (when-some [x (<! cmds)]
         (if (instance? Command x)
           (case (:type x)
             :open        (do
                            (fire-events registry :open)
                            (recur (second x) registry buf))
             :close       (do
                            (fire-events registry :close)
                            (recur nil subs buf))
             :subscribe   (let [{:keys [event key fn]} (:data x)]
                            (recur ws (assoc-in registry [event key] fn) buf))
             :unsubscribe (let [{:keys [event key]} (:data x)]
                            (recur ws (update registry event dissoc key) buf)))
           (if ws
             (do
               (doseq [x buf]
                 (<! (send! ws x)))
               (<! (send! ws x))
               (recur ws registry []))
             (recur ws registry (conj buf x))))))

     ((fn connect []
        (a/take! (websocket-client req (a/promise-chan) exception-handler)
                 (fn [ws]
                   (when ws
                     (websocket-connect ws in false)
                     (a/put! cmds (Command. :open ws))
                     (s/on-closed
                      ws (fn []
                           (a/put! cmds (Command. :close ws))
                           (if (and reconnect? (not (clojure.core.async.impl.protocols/closed? duplex)))
                             (connect)
                             (debug "websocket closed" req))))))))))
   duplex))

(defn subscribe [duplex event key f]
  (a/put! duplex (Command. :subscribe {:event event :key key :fn f})))

(defn unsubscribe [duplex event key]
  (a/put! duplex (Command. :unsubscribe {:event event :key key})))

(defn on-open [duplex key f]
  (subscribe duplex :open key f))

(defn on-close [duplex key f]
  (subscribe duplex :close key f))

