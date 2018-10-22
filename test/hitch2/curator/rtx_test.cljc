(ns hitch2.curator.rtx-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing use-fixtures]]
               :clj [clojure.test :refer [deftest is testing use-fixtures]])
            [hitch2.curator.rtx :as rtx]
            [hitch2.test-common :refer [Constant]]
            [hitch2.selector :as sel]
            [hitch2.protocols.selector :as sel-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.graph :as h]
            [hitch2.curator.mutable-var :refer  [mutable-var]]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.selector-impl-registry :as reg :refer [registry-resolver]]))

(def results (atom []))

(defn fake-react-effect-handler [f]
  (defmethod graph-proto/run-effect :rerender-components
    [gm effect]
    (swap! results conj effect))
  (f) ;; run tests
  (remove-method graph-proto/run-effect :rerender-components))

(use-fixtures :once fake-react-effect-handler)

(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm registry-resolver))]])

(defn render-function [value rtx services]
  value)

(doseq [[gname gctor] gctors]
  (deftest doesnt-blow-up
    (let [g        (gctor)
          services {:graph g}
          value    3]
      (reset! results [])
      (is (= value (rtx/qui-hitch g :nf :component render-function value services)))
      (is (= [] @results)))))


(defn render-with-deps [value rtx services]
  @(h/select-sel! rtx value))

(doseq [[gname gctor] gctors]
  (deftest satisfiable-parents
    (let [g        (gctor)
          services {:graph g}
          mv-sel    (mutable-var :mv)]
      (reset! results [])
      (is (= :nf (rtx/qui-hitch g :nf :component render-with-deps mv-sel services)))
      (is (= [] @results))

      (h/apply-commands g [[mv-sel [:set-value 42]]])
      (reset! results [])

      (is (= 42 (rtx/qui-hitch g :nf :component render-with-deps mv-sel services)))
      (is (= [] @results))

      (h/apply-commands g [[mv-sel [:set-value 7]]])
      (is (= [{:type :rerender-components, :components #{:component}}]
             @results)))))
