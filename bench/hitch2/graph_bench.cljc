(ns hitch2.graph-bench
  (:require [hitch2.def.halting :refer [defhalting]]
            [hitch2.graph :as api]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.protocols.graph-manager :as gm-proto]
            [hitch2.curator.mutable-var :as mv]
            [hitch2.selector-impl-registry :as reg
             :refer [registry-resolver]]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
    #?(:clj
            [criterium.core :refer [bench]]
       :cljs [figwheel.main.async-result :as async-result])))

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

(def-descriptor-spec fib-map-spec
  :not-machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/map)

(defn fibmap-halting [G {:keys [n] :as sel}]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (api/select-sel! G (assoc sel :n (dec n)))
              n-2 (api/select-sel! G (assoc sel :n (dec (dec n))))]
          (+ @n-1 @n-2))))

(def fibimpl {:hitch2.descriptor.impl/kind                  :hitch.selector.kind/halting
              :hitch.selector.impl/halting               fibmap-halting
              :hitch.selector.impl/halting-slot-selector fibmap-halting})

(reg/def-registered-selector fib-map-spec' fib-map-spec fibimpl)
(declare fibb-graph)

(defhalting fibb-graph [G n]
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

(def-descriptor-spec depends-on-map-spec
  :not-machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/map)

(defn depends-on-map-halting [G {:keys [n] :as sel}]
  (cond (= 0 n) @(api/select-sel! G (mv/mutable-var :bench))
        :else   (+ 1 @(api/select-sel! G (assoc sel :n (dec n))))))

(def depends-on-map-impl
  {:hitch2.descriptor.impl/kind                  :hitch.selector.kind/halting
   :hitch.selector.impl/halting               depends-on-map-halting
   :hitch.selector.impl/halting-slot-selector depends-on-map-halting})

(reg/def-registered-selector depends-on-map-spec' depends-on-map-spec depends-on-map-impl)

(declare depends-on)
(defhalting depends-on [G n]
  (cond (= 0 n) @(api/select-sel! G (mv/mutable-var :bench))
        :else   (+ 1 @(api/select! G depends-on (dec n)))))

(defn depends-bench [bench-name sel]
  (test-header bench-name)
  (binding [atom-gm/*trace* false]
    (atom-gm/clear-trace!)
    (let [g (atom-gm/make-gm registry-resolver)]

      (api/hook-sel g
                    (fn [result]
                      (prn "value is: " result))
                    sel)
      (api/apply-commands g [[(mv/mutable-machine :bench) [:set-value 4000]]])
      (api/hook-sel g
                    (fn [result]
                      (prn "value is now: " result))
                    sel))))

(defn deep-value-change-bench [bench-name sel]
  (test-header bench-name)
  (let [g (atom-gm/make-gm registry-resolver)
        machine-sel (mv/mutable-machine :bench)]
    (api/pin g sel)

    #?(:cljs (simple-benchmark []
               (api/apply-commands g [[machine-sel [:set-value (rand-int 54)]]])
               bench-times)
       :clj  (bench (api/apply-commands g [[machine-sel [:set-value (rand-int 54)]]])))))
(defn runner [f]
  #?(:cljs  (js/setTimeout #(async-result/send (f)) 0)
     :clj (f)))

(defn bench-runner []
  (depends-bench "depends-record " (descriptor/dtor  depends-on 100))
  (depends-bench "depends-map" (descriptor/map->dtor
                                 depends-on-map-spec'
                                 {:n   100}))
  (deep-value-change-bench  "deep-value-change-bench-record"
    (descriptor/dtor  depends-on 10))
  (deep-value-change-bench  "deep-value-change-bench-map"
    (descriptor/map->dtor
      depends-on-map-spec'
                                                            {:n   10}))
  (fib-bench "fib-record" (fn [] (descriptor/dtor  fibb-graph 40)))
  (fib-bench "fib-map" (fn []
                         (descriptor/map->dtor
                           fib-map-spec'
                           {:n   40}))))

(defn -main [& args]
  (runner bench-runner)

  #?(:cljs [:figwheel.main.async-result/wait 100000]))
