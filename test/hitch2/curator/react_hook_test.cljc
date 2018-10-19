(ns hitch2.curator.react-hook-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing use-fixtures]]
               :clj [clojure.test :refer [deftest is testing use-fixtures]])
            [hitch2.curator.react-hook :as rh]
            [hitch2.test-common :refer [Constant]]
            [hitch2.selector :as sel]
            [hitch2.protocols.selector :as sel-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.graph :as graph]
            [hitch2.curator.mutable-var :refer  [mutable-var]]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.selector-impl-registry :as reg
             :refer [registry-resolver]]))

(def results (atom []))

(defn fake-effect-handler [f]
  (defmethod graph-proto/run-effect :rerender-components
    [gm effect]
    (swap! results conj effect))
  (f) ;; run tests
  (remove-method graph-proto/run-effect :rerender-components))

(use-fixtures :once fake-effect-handler)

(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm registry-resolver))]])

(defn reset-rc-parents [gm rc new-parents]
  (graph-proto/-transact! gm rh/react-hooker [:reset-component-parents rc new-parents]))

(doseq [[gname gctor] gctors]
  (deftest simple-get-ok
    (let [g (gctor)]

      (reset! results [])

      (reset-rc-parents g :react-component #{(Constant 0)})

      (is (= @results [{:type :rerender-components, :components #{:react-component}}]))
      (reset! results []))))
