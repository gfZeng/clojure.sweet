(ns sweet.io.http.java
  (:import [java.nio ByteBuffer]
           [java.util LinkedList]
           [java.util.concurrent CompletionStage]
           [java.net URI]
           [java.net.http HttpClient WebSocket
            WebSocket$Builder WebSocket$Listener])
  (:require [sweet.io.http :as http]))

(defn ByteBuffer-concat [buffers]
  (let [len             (apply +  0 (map #(.remaining ^ByteBuffer %) buffers))
        ^ByteBuffer buf (ByteBuffer/allocate len)]
    (doseq [^ByteBuffer b buffers]
      (.put buf b))
    buf))

(extend-type WebSocket
  http/WebSocketConnection
  (ping! [this message]
    (.sendPing this ^ByteBuffer message))
  (pong! [this message]
    (.sendPong this ^ByteBuffer message))
  (close! [this code reason]
    (.sendClose this code reason))
  (closed? [this]
    (and (.isOutputClosed this)
         (.isInputClosed this)))
  (send! [^WebSocket this message]
    (if (instance? ByteBuffer message)
      (.sendBinary this ^ByteBuffer message true)
      (.sendText this ^CharSequence message true))))

(defn ^WebSocket$Listener make-listener
  [{:keys [on-open on-close on-error on-ping on-pong on-text on-binary]}]
  (let [^LinkedList buffers (LinkedList.)]
    (reify WebSocket$Listener

      (^void onOpen [this ^WebSocket conn]
       (.request conn 1)
       (on-open conn)
       nil)

      (^CompletionStage onClose [this ^WebSocket conn ^int code ^String reason]
       (on-close conn code reason)
       nil)

      (^void onError [this ^WebSocket conn ^Throwable error]
       (on-error conn error)
       nil)

      (^CompletionStage onPing [this ^WebSocket conn ^ByteBuffer buf]
       (.request conn 1)
       (if on-ping
         (on-ping conn buf)
         (.sendPong conn buf))
       nil)

      (^CompletionStage onPong [this ^WebSocket conn ^ByteBuffer buf]
       (.request conn 1)
       (when on-pong (on-pong conn buf))
       nil)

      (^CompletionStage onBinary [this ^WebSocket conn ^ByteBuffer buf ^boolean last?]
       (.request conn 1)
       (.addLast buffers buf)
       (when last?
         (let [buf (ByteBuffer-concat buffers)]
           (.clear buffers)
           (on-binary conn buf)))
       nil)

      (^CompletionStage onText [this ^WebSocket conn ^CharSequence text ^boolean last?]
       (.request conn 1)
       (.addLast buffers text)
       (when last?
         (let [len               (apply + 0 (map #(.length ^CharSequence %) buffers))
               ^StringBuilder sb (StringBuilder. len)]
           (doseq [^CharSequence buf buffers]
             (.append sb buf))
           (.clear buffers)
           (on-text conn (.toString sb))))
       nil))))

(defn ^WebSocket$Builder make-builder
  [{:keys [subprotocols headers]}]
  (let [^WebSocket$Builder builder (.newWebSocketBuilder
                                    (HttpClient/newHttpClient))]
    (when-some [[^String p & ps] (seq subprotocols)]
      (.subprotocols builder p (into-array String ps)))
    (doseq [[k v] headers]
      (.header builder (name k) v))
    builder))

(defmethod http/websocket-connection :java [_ request listener]
  (let [builder  (make-builder request)
        listener (make-listener listener)
        ^URI uri (URI/create (:url request))]
    (.get (.buildAsync builder uri listener))))



