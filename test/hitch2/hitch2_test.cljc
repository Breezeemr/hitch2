(ns hitch2.hitch2-test
    (:require
      #?(:cljs [cljs.test :refer [deftest is testing]]
         :clj [clojure.test :refer [deftest is testing]])
      [hitch2.machine.dependent-get :refer [dget-machine]]
      [hitch2.protocols.machine :as machine-proto]
      [clojure.data :as diff]))

(def sel-impl {:s-name :hi
               :kind :hitch.selector.kind/halting
               :halting (fn [g sel] :hi)})

(deftest dget-machine-test
  (let [node (machine-proto/-initialize dget-machine)
        sel {:s-name sel-impl}]
    (is (= node (assoc machine-proto/initial-node :state {})))
    (let [node (machine-proto/-apply-command dget-machine
                  {}  node #{} #{}
                  [:dget-subscribe sel :target])
          [f s both] (diff/diff node
                       (machine-proto/->node-state {sel #{:target}} {sel true} {} [] []))]
      (is (nil? f))
      (is (nil? s)))
    ))
