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
    #?(:cljs (simple-benchmark [selector {:s-name         :name-of-selector
                                          :patient-id     4
                                          :encounter-date (js/Date.)}]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn -main []
  (bench-vectors)
  (bench-records)
  (bench-maps))
