(ns hitch2.defselector-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
            [hitch2.selector :refer [defselector]]
            [hitch2.protocols.selector :as sel-proto]
            [hitch2.sel :as sel]))

(deftest DefSelector
  (testing "defselector definition does not error-out"
    (defselector TEST-SELECTOR [G A [B1 B2 :as B] {C1 :k :as C}]
      [A B C]))
  (testing "defselector eval-fn generation"
    (is (= (TEST-SELECTOR-eval-fn nil {:value {:A 1 :B [2] :C {:k 3} } }) [1 [2] {:k 3}])))
  (testing "defselector record property names are correct"
    (let [{{a :A b :B c :C} :value} (sel/sel TEST-SELECTOR 1 [2] {:k 3})]
      (is (= a 1))
      (is (= b [2]))
      (is (= c {:k 3}))))
  (testing "defselector can destructure maps"
    (defselector overview-items [g {:keys [a b] :as args}
                                 C]
      (+ a b C))
    (is (= (overview-items-eval-fn :graph {:value {:args {:a 1 :b 1}
                                                   :C    1}})
           3))
    (let [{{{a :a b :b} :args
            C           :C} :value} (sel/sel overview-items {:a 1 :b 2} 3)]
      (is (= a 1))
      (is (= b 2))
      (is (= C 3)))))
