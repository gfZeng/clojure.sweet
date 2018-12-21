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
     (let [xs# (transient [(<! ch#)])]
       (loop [n# (dec n#)]
         (if-some [x# (when (pos? n#)
                        (poll! ch#))]
           (do (conj! xs# x#)
               (recur (dec n#)))
           (persistent! xs#))))))

(defn take<!! [n ch]
  (<!! (go (take<! n ch))))

(defmacro <<! [ch]
  `(some-> (<! ~ch) (<!)))
