(ns ^:skip-aot? sweet.io.kafka
  (:refer-clojure :exclude [send])
  (:require [clojure.tools.logging :as log
             :refer (debug info warn error fatal spy)]
            [clojure.core.async :as a
             :refer (go go-loop <! >! chan timeout <!!)]
            [clojure.java.io :as io])
  (:import [java.util Map]
           [java.time Duration]
           [java.sql Timestamp]
           [java.io ByteArrayOutputStream]
           [clojure.lang PersistentHashMap]

           [org.apache.kafka.common TopicPartition]
           [org.apache.kafka.common.errors WakeupException]
           [org.apache.kafka.common.serialization
            Serializer Deserializer
            StringSerializer StringDeserializer]
           [org.apache.kafka.clients.producer
            KafkaProducer ProducerRecord Callback
            RecordMetadata]
           [org.apache.kafka.clients.consumer
            ConsumerRebalanceListener
            KafkaConsumer ConsumerRecords ConsumerRecord
            OffsetAndMetadata OffsetCommitCallback]))

(declare seek subscribe)

(def SERVERS #prop [kafka.servers "127.0.0.1:9092"])

(def CONSUMER-SPEC
  {"bootstrap.servers"  SERVERS
   "key.deserializer"   "org.apache.kafka.common.serialization.StringDeserializer"
   "enable.auto.commit" false})

(def PRODUCER-SPEC
  {"bootstrap.servers"  SERVERS
   "max.block.ms"       (str Long/MAX_VALUE)
   "max.request.size"   "2097152"
   "key.serializer"     "org.apache.kafka.common.serialization.StringSerializer"
   "enable.idempotence" "true"})

(def ^:dynamic *options* {})

(def ^Duration ten-seconds (Duration/ofSeconds 10))

(defn part ^TopicPartition [topic no]
  (TopicPartition. topic no))

(defn consumer ^KafkaConsumer
  ([]         (consumer nil))
  ([offsets]  (consumer offsets StringDeserializer))
  ([offsets decoder]
   (let [^Deserializer decoder
         (if (instance? Deserializer decoder)
           decoder
           (let [coerce (or decoder (throw (Exception. "Muse provide decoder")))]
             (reify Deserializer
               (configure [_ _ _])
               (close [_])
               (deserialize [_ _ bs]
                 (coerce bs)))))
         spec (merge CONSUMER-SPEC *options*)]
     (if (Boolean/valueOf (spec "enable.auto.commit"))
       (doto (KafkaConsumer. spec nil decoder)
         (subscribe offsets))
       (doto (KafkaConsumer. spec nil decoder)
         (seek offsets))))))


(defn consumer-command!
  [^KafkaConsumer consumer {:as cmd :keys [op args callback]}]
  (case op
    :seek   (do
              (.seek consumer (first args) (second args))
              (when callback (callback)))
    :commit (.commitAsync consumer {(first args) (OffsetAndMetadata. (second args))}
                          (reify OffsetCommitCallback
                            (^void onComplete [_ ^Map offsets ^Exception e]
                             (when e (throw e))
                             (when callback (callback)))))
    (throw (ex-info "No implements" {:cmd cmd}))))

(defn- duplex-poll! [^KafkaConsumer consumer ^Duration duration]
  (try
    (->> ^ConsumerRecords (.poll consumer duration)
         (.iterator)
         (iterator-seq))
    (catch WakeupException e)))

(defn- run-consumer-commands! [consumer wakeups]
  (->> (repeatedly #(a/poll! wakeups))
       (take-while some?)
       (run! #(consumer-command! consumer %))))

(defn duplex
  ([offsets]    (duplex offsets (chan)))
  ([offsets in] (duplex offsets in nil))
  ([offsets in decoder]
   (let [^KafkaConsumer consumer (consumer offsets decoder)
         ^Duration duration      (Duration/ofMillis 10000)

         wakeups (a/chan 1 (map #(do (.wakeup consumer) %)))]
     (doto (Thread. (fn []
                      (if-some [records (duplex-poll! consumer duration)]
                        (loop [rs (seq records)]
                          (when rs
                            (a/alt!!
                              :priority true
                              [[in (first rs)]] (recur (next rs))
                              wakeups
                              ([cmd]
                               (consumer-command! consumer cmd)
                               (run-consumer-commands! consumer wakeups)
                               (recur rs)))))
                        (do
                          (run-consumer-commands! consumer wakeups)
                          (warn "No records after waiting" (.getSeconds duration) "secs")))
                      (recur)))
       (.setName "consumer-polling")
       ;; (.setDaemon true)
       (.start))
     (a/duplex in wakeups))))

(defn producer
  ([] (producer StringSerializer))
  ([encoder]
   (let [^Serializer encoder
         (if (instance? Serializer encoder)
           encoder
           (let [coerce (or encoder (throw (Exception. "Must provide encoder")))]
             (reify Serializer
               (configure [_ _ _])
               (close [_])
               (serialize [_ _ x]
                 (coerce x)))))]
     (KafkaProducer. PRODUCER-SPEC nil encoder))))


(defn producer-record [topic part-no key value]
  (ProducerRecord. topic part-no key value))

(defn send
  ([producer topic part-no key value]
   (send producer topic part-no key value (constantly nil) #(error %)))
  ([producer topic part-no key value success!]
   (send producer topic part-no key value success! #(error %)))
  ([producer topic part-no key value success! error!]
   (.send producer
          (producer-record topic part-no key value)
          (reify Callback
            (^void onCompletion [_ ^RecordMetadata mt ^Exception e]
             (if e (error! e) (success! mt)))))))

(defn send!
  ([producer topic part-no key value]
   (send! producer topic part-no key value (a/promise-chan)))
  ([producer topic part-no key value pch]
   (send producer topic part-no key value
          #(a/put! pch %)
          #(do (a/close! pch) (error %) (throw %)))
   pch))

(defn end-offsets [consumer parts]
  (PersistentHashMap/create (.endOffsets consumer parts)))

(defn end-offset [consumer part]
  (get (end-offsets [part]) part))

(defn seek [consumer offsets]
  (.assign consumer (vec (keys offsets)))
  (doseq [[part offset] offsets]
    (if (neg? offset)
      (.seekToEnd consumer [part])
      (.seek consumer part offset))))

(defn subscribe [consumer offsets]
  (.subscribe
   consumer (map #(.topic %) (keys offsets))
   (reify ConsumerRebalanceListener
     (onPartitionsRevoked [_ parts]
       (info "revoked partitions" parts))
     (onPartitionsAssigned [_ parts]
       (info "assigned partitions" parts)
       (doseq [part  parts
               :let  [offset (offsets part)]
               :when (not (neg? offset))]
         (.seek consumer part offset))))))

(defn poll! [consumer offsets]
  (when (seq offsets)
    (seek consumer offsets))
  (->> (.poll consumer ten-seconds)
       (.iterator)
       (iterator-seq)))

(defn of-offset [consumer part offset]
  (first (poll! {part offset})))



;;; utils for service
(defonce PRODUCER (delay (producer)))
