(ns hitch2.hitch2-test
    (:require
      #?(:cljs [cljs.test :refer [deftest is testing]]
         :clj [clojure.test :refer [deftest is testing]])
      [hitch2.machine.pin :refer [pin-machine]]
      [hitch2.protocols.machine :as machine-proto]
      [hitch2.protocols.selector :as selector-proto]
      [clojure.data :as diff]))

(def sel-impl {:s-name :hi
               :kind :hitch.selector.kind/halting
               :halting (fn [g sel] :hi)})

(deftest dget-machine-test
  (let [impl (selector-proto/-imp pin-machine)
        node (machine-proto/-initialize impl pin-machine)]
    (is (= node (assoc machine-proto/initial-machine-state :state #{})))
    #_(let [node (machine-proto/-apply-command pin-machine
                  {}  node #{} #{}
                  [:dget-subscribe sel :target])
          [f s both] (diff/diff node
                       (machine-proto/->machine-state {sel #{:target}} {sel true} {} [] []))]
      (is (nil? f))
      (is (nil? s)))
    ))
