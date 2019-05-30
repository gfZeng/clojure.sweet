(in-ns 'clojure.core.async)
(require '[clojure.core.async.impl.protocols :as impl])

(defn duplex [in out]
  (reify
    impl/Channel
    (close! [_]
      (impl/close! in)
      (impl/close! out))
    (closed? [_]
      (and
       (impl/closed? in)
       (impl/closed? out)))

    impl/ReadPort
    (take! [_ fn]
      (impl/take! in fn))

    impl/WritePort
    (put! [_ v fn]
      (impl/put! out v fn))

    clojure.lang.ILookup
    (valAt [this k]
      (.valAt this k nil))
    (valAt [_ k not-found]
      (case k
        :in  in
        :out out
        not-found))))

(defmacro take<! [n ch]
  `(let [n#  ~n
         ch# ~ch]
     (assert (pos? n#) ~(str n " must greater than 0"))
     (when-some [x# (<! ch#)]
       (let [xs# (transient [x#])]
         (loop [n# (dec n#)]
           (if-some [x# (when (pos? n#)
                          (poll! ch#))]
             (do (conj! xs# x#)
                 (recur (dec n#)))
             (persistent! xs#)))))))

(defn take<!! [n ch]
  (<!! (go (take<! n ch))))

(defmacro <<! [ch]
  `(some-> (<! ~ch) (<!)))

(defmacro go-promise
  {:style/indent 0}
  [& body]
  `(let [pch# (promise-chan)]
     (go
       (let [ret# (do ~@body)]
         (if (nil? ret#)
           (close! pch#)
           (put! pch# ret#))))
     pch#))

(deftype ReducingBuffer [^:volatile-mutable rval ^:volatile-mutable ^long n rf]
  impl/UnblockingBuffer
  impl/Buffer
  (full? [this]
    false)
  (remove! [this]
    (when (== n 1)
      (set! n 0)
      (let [val rval]
        (set! rval (rf))
        val)))
  (add!* [this itm]
    (set! n 1)
    (set! rval (rf rval itm))
    this)
  (close-buf! [this])

  clojure.lang.Counted
  (count [this] n))


(defn reducing-buffer [rf]
  (ReducingBuffer. (rf) 0 rf))
