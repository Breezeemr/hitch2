(ns hitch2.graph-bench
  (:require [hitch2.selector :as sel]
            [hitch2.graph :as api]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.protocols.graph-manager :as gm-proto]
            [hitch2.machine.mutable-var :as mv]
            [hitch2.selector-impl-registry :as reg
             :refer [registry-resolver]]
    #?(:clj
            [criterium.core :refer [bench]])))

(def bench-times 100000)
(defn test-header [subject]
  (println "***********************")
  (println "***********************    " subject)
  (println "***********************"))
;; 1. constructs a selector in the graph X times
;; 2. one construct each time and select in the grpah X times
;; 3. then do that with a deep selector: a selector that selects a selector that selects many selectors

(defn fibb-no-graph [n]
  (cond (= 0 n) 0
        (= 1 n) 1
        :else (+ (fibb-no-graph (dec n))
                 (fibb-no-graph (dec (dec n))))))

(defn fibmap [G {:keys [n] :as sel}]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (api/select-sel! G (assoc sel :n (dec n)))
              n-2 (api/select-sel! G (assoc sel :n (dec (dec n))))]
          (+ @n-1 @n-2))))

(def fibimpl {:hitch.selector.impl/kind :hitch.selector.kind/halting
           :halting                     fibmap})

(reg/def-registered-selector fib-x ::fibmap fibimpl)
(declare fibb-graph)

(sel/defselector fibb-graph [G n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (api/select! G fibb-graph (dec n))
              n-2 (api/select! G fibb-graph (dec (dec n)))]
          (+ @n-1 @n-2))))

(defn fib-bench [bench-name sel-fn]
  (test-header bench-name)
  (let [g (atom-gm/make-gm registry-resolver)]
    #?(:cljs (simple-benchmark []
               (let [sel (sel-fn)]
                 (api/pin g sel)
                   #_(prn :pined
                       (get (gm-proto/-get-graph g) sel))
                   (api/unpin g sel))
               bench-times
               #_(prn :unpined
                 (get (gm-proto/-get-graph g) sel)))
       :clj  (bench
               (let [sel (sel-fn)]
                 (api/pin g sel)
                   #_(prn :pined
                       (get (gm-proto/-get-graph g) sel))
                   (api/unpin g sel))
               #_(prn :unpined
                   (get (gm-proto/-get-graph g) sel))))))

(defn depends-on-map [G {:keys [n] :as sel}]
  (cond (= 0 n) @(api/select! G mv/mutable-var :bench)
        :else   (+ 1 @(api/select-sel! G (assoc sel n (dec n))))))

(def depends-on-map-impl
  {:hitch.selector.impl/kind :hitch.selector.kind/halting
   :halting                  depends-on-map})

(reg/def-registered-selector depends-on-map-x ::depends-on-map depends-on-map-impl)

(declare depends-on)
(sel/defselector depends-on [G n]
  (cond (= 0 n) @(api/select! G mv/mutable-var :bench)
        :else   (+ 1 @(api/select! G depends-on (dec n)))))

(defn depends-bench [n]
  (binding [atom-gm/*trace* false]
    (atom-gm/clear-trace!)
    (let [g (atom-gm/make-gm registry-resolver)]

      (api/hook-sel g
                    (fn [result]
                      (prn "value is: " result))
                    (depends-on n))
      (api/apply-commands g [[(mv/->mutable-machine :bench) [:set-value 5000]]])
      (api/hook-sel g
                    (fn [result]
                      (prn "value is now: " result))
                    (depends-on n)))))

(defn deep-value-change-bench [bench-name sel]
  (test-header bench-name)
  (let [g (atom-gm/make-gm registry-resolver)
        machine-sel (mv/->mutable-machine :bench)]
    (api/pin g sel)

    #?(:cljs (simple-benchmark []
               (api/apply-commands g [[machine-sel [:set-value (rand-int 54)]]])
               bench-times)
       :clj  (bench (api/apply-commands g [[machine-sel [:set-value (rand-int 54)]]])))))

(defn -main []
  (deep-value-change-bench  "deep-value-change-bench-record" (depends-on 100))
  (deep-value-change-bench  "deep-value-change-bench-map" {:hitch.selector/name depends-on-map-x
                                                           :n                   100})
  (fib-bench "fib-record" (fn [] (fibb-graph 40)))
  (fib-bench "fib-map" (fn [] {:hitch.selector/name fib-x
                               :n                   40})))
