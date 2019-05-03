(ns sweet.reader
  (:require [clojure.string :as str]
            [clojure.tools.logging :as log
             :refer (debug info warn error fatal spy)]))


(def COERCIONS
  {'int     #(Integer/parseInt %)
   'long    #(Long/parseLong %)
   'boolean #(Boolean/parseBoolean %)
   'strings #(vec (str/split % #","))
   'ints    (fn [s] (mapv #(Integer/parseInt %)
                          (str/split s #",")))
   'longs   (fn [s] (mapv #(Long/parseLong %)

                          (str/split s #",")))})

(defn read-property
  ([x] (read-property x nil))
  ([x not-found]
   (let [tag  (:tag (meta x))
         k    (str x)
         envk (str/upper-case (str/replace k #"[-.]" "_"))
         v    (or (System/getProperty k)
                  (System/getenv envk))
         prop (if (nil? v)
                not-found
                ((COERCIONS tag identity) v))]
     (info "#prop" k "=>" (pr-str prop))
     prop)))

(defn read-property-form [x]
  (if (sequential? x)
    `(read-property '~(first x) ~(second x))
    `(read-property '~x nil)))

(defn array-type [clazz]
  (case clazz
    boolean (Class/forName "[Z")
    byte    (Class/forName "[B")
    char    (Class/forName "[C")
    short   (Class/forName "[S")
    int     (Class/forName "[I")
    long    (Class/forName "[J")
    float   (Class/forName "[F")
    double  (Class/forName "[D")
    (Class/forName (str "[L" (.getCanonicalName (resolve clazz)) ";"))))

