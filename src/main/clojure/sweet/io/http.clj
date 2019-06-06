(ns sweet.io.http
  (:require [clojure.tools.logging :as log
             :refer (debug info warn error fatal spy)]))


(def ^:const reconnect-code 3777)

(defn default-error-handler [conn e]
  (error e "connection" conn)
  (throw e))

(defmulti websocket-connection
  (fn [impl-keyword request listener] impl-keyword))

(defprotocol WebSocketConnection
  (closed? [this] closed?)
  (close!  [this] [this code reason])
  (send!   [this message])
  (ping!   [this message])
  (pong!   [this message]))

(defprotocol Reconnection
  (connection [this])
  (listen! [this listener])
  (once-opened [this key handler])
  (remove-once-opened [this key])
  (reconnect! [this] [this previous-connection]))



(deftype WebSocket [impl-keyword
                    ^:volatile-mutable connection
                    request listener
                    closed?
                    once-opened-handlers]

  WebSocketConnection
  (closed? [this] closed?)
  (close! [this]
    (set! (.-closed? this) true)
    (close! connection))
  (close! [this code reason]
    (set! (.-closed? this) true)
    (close! connection code reason))
  (send! [this message]
    (send! connection message))

  Reconnection
  (connection [this] connection)
  (listen! [this listener]
    (let [open-fn  (:on-open listener)
          close-fn (:on-close listener)
          on-open  (fn [conn]
                     (let [ret (when open-fn (open-fn conn))]
                       (doseq [f (vals @once-opened-handlers)]
                         (f conn))
                       ret))
          on-close (fn [conn code reason]
                     (let [ret (when close-fn (close-fn conn code reason))]
                       (when (or (identical? ret :reconnect)
                                 (== code reconnect-code))
                         (reconnect! this conn))
                       ret))
          on-error (or (:on-error listener) default-error-handler)
          listener (assoc listener
                          :on-open on-open
                          :on-close on-close
                          :on-error on-error)]
      (set! (.-listener this) listener)
      this))
  (once-opened [this key handler]
    (locking this
      (vswap! once-opened-handlers assoc key handler)
      (handler connection))
    this)

  (remove-once-opened [this key]
    (locking this
      (vswap! once-opened-handlers dissoc key))
    this)

  (reconnect! [this]
    (close! connection reconnect-code "reconnect"))

  (reconnect! [this previous-connection]
    (when previous-connection (close! previous-connection))
    (when (and (not closed?) (identical? previous-connection connection))
      (locking this
        (when (identical? previous-connection connection)
          (set! (.-connection this)
                (websocket-connection impl-keyword request listener)))))
    this))

(def ^:const default-implement :java)

(defn websocket
  ([request listener]
   (websocket default-implement request listener))

  ([impl-keyword request listener]
   (doto (WebSocket. impl-keyword nil request nil false (volatile! {}))
     (listen! listener)
     (reconnect! nil))))


(load "http/java")
