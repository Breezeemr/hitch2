(ns hitch2.selectors-bench
  (:require [hitch2.protocols.selector :as s]
            #?(:clj [criterium.core :refer [bench]])))

(def bench-times 100000)

(defn date []
  #?(:cljs (js/Date.) :clj (java.util.Date.)))

(defn test-header [subject]
  (println "***********************")
  (println "***********************    " subject)
  (println "***********************"))

(defn bench-vectors []
  (test-header "vectors")
  (let [selector [:name 4 (date)]
        f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn bench-records []
  (test-header "records")
  (let [selector (s/->Selector2 :name 4 (date))
        f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
               (s/-invoke-halting selector f gv)
               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn bench-maps []
  (test-header "records invoked with map")
  (let [selector {:s-name         :thing
                  :customer-id    4
                  :encounter-date (date)}
        f        (fn [gv-tracker {:keys [customer-id encounter-date]}] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))

(defn bench-types []
  (test-header "deftype selector")
  (let [selector (s/->TypeSelector1 :name :value)
        f        (fn [gv-tracker value] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark [selector selector]
                               (s/-invoke-halting selector f gv)
                               bench-times)
       :clj (bench (s/-invoke-halting selector f gv)))))



(defn bench-vectors-with-construction []
  (test-header "vectors")
  (let [f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark []
               (s/-invoke-halting [:name 4 (date)] f gv)
               bench-times)
       :clj (bench (s/-invoke-halting [:name 4 (date)] f gv)))))

(defn bench-records-with-construction []
  (test-header "records")
  (let [f        (fn [gv-tracker customer-id encounter-date] :do-stuff)
        gv       {:fake :gv-tracker}]
    #?(:cljs (simple-benchmark []
               (s/-invoke-halting (s/->Selector2 :name 4 (date)) f gv)
               bench-times)
       :clj (bench (s/-invoke-halting (s/->Selector2 :name 4 (date)) f gv)))))

(defn bench-maps-with-construction []
  (test-header "maps invoked with the map: ")
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
  (bench-maps)
  (bench-types))
