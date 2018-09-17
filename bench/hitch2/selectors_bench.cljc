(ns hitch2.selectors-bench
  (:require [hitch2.protocols.selector :as s]
            #?(:clj [criterium.core :refer [bench]])))

(def bench-times 100000)

(defn date []
  #?(:cljs (js/Date.) :clj (java.util.Date.)))

(defn bench-vectors []
  (println "benchmarking vectors: ")
  (let [selector [:name 4 (date)]
        f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn bench-records []
  (println "benchmarking records: ")
  (let [selector (s/->Selector2 :name 4 (date))
        f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
               (s/-invoke-halting selector f gv)
               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn bench-maps []
  (println "benchmarking maps invoked with the map: ")
  (let [selector {:s-name         :thing
                  :customer-id    4
                  :encounter-date (date)}
        f        (fn [gv-tracker {:keys [customer-id encounter-date]}] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))



(defn bench-vectors-with-construction []
  (println "benchmarking vectors: ")
  (let [f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark []
               (s/-invoke-halting [:name 4 (date)] f gv)
               bench-times)
       :clj (bench (s/-invoke-halting [:name 4 (date)] f gv)))))

(defn bench-records-with-construction []
  (println "benchmarking records: ")
  (let [f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark []
               (s/-invoke-halting (s/->Selector2 :name 4 (date)) f gv)
               bench-times)
       :clj (bench (s/-invoke-halting (s/->Selector2 :name 4 (date)) f gv)))))

(defn bench-maps-with-construction []
  (println "benchmarking maps invoked with the map: ")
  (let [f        (fn [gv-tracker {:keys [customer-id encounter-date]}] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark []
               (s/-invoke-halting {:s-name         :thing
                                   :customer-id    4
                                   :encounter-date (date)} f gv)
               bench-times)
       :clj (bench (s/-invoke-halting {:s-name         :thing
                                       :customer-id    4
                                       :encounter-date (date)} f gv)))))


(defn -main []
  (bench-vectors-with-construction)
  (bench-records-with-construction)
  (bench-maps-with-construction)
  (bench-vectors)
  (bench-records)
  (bench-maps))
