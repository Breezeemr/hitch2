(ns hitch2.graph-bench
  (:require [hitch2.selector :as sel]
            [hitch2.graph :as api]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.machine.mutable-var :as mv]))

;; 1. constructs a selector in the graph X times
;; 2. one construct each time and select in the grpah X times
;; 3. then do that with a deep selector: a selector that selects a selector that selects many selectors

(defn fibb-no-graph [n]
  (cond (= 0 n) 0
        (= 1 n) 1
        :else (+ (fibb-no-graph (dec n))
                 (fibb-no-graph (dec (dec n))))))

(declare fibb-graph)

(sel/defselector fibb-graph [G n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (let [n-1 (api/select! G fibb-graph (dec n))
              n-2 (api/select! G fibb-graph (dec (dec n)))]
          (+ @n-1 @n-2))))

(defn fib-bench [n]
  (binding [atom-gm/*trace* true]
    (atom-gm/clear-trace!)
    (let [g (atom-gm/make-gm)]
      (api/hook-sel g
                    (fn [result]
                      (prn "*******************************")
                      (prn "got result: " result))
                    (fibb-graph n)))))

(declare depends-on)
(sel/defselector depends-on [G n]
  (cond (= 0 n) @(api/select! G mv/mutable-var :bench)
        :else   (+ 1 @(api/select! G depends-on (dec n)))))

(defn depends-bench [n]
  (binding [atom-gm/*trace* true]
    (atom-gm/clear-trace!)
    (let [g (atom-gm/make-gm)]
      (api/apply-commands g [[(mv/mutable-var :bench) [:set-value 0]]])
      (api/hook-sel g
                    (fn [result]
                      (prn "value is: " result))
                    (depends-on 1000))
      (api/apply-commands g [[(mv/mutable-var :bench) [:set-value 5000]]])
      (api/hook-sel g
                    (fn [result]
                      (prn "value is now: " result))
                    (depends-on 1000)))))
