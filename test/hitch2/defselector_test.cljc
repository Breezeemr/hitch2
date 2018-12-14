(ns hitch2.defselector-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
            [hitch2.def.halting :refer [defhalting]]
            [hitch2.descriptor :as descriptor] ))

(deftest DefSelector
  (testing "defhalting definition does not error-out"
    (defhalting TEST-SELECTOR [G A [B1 B2 :as B] {C1 :k :as C}]
      [A B C]))
  (testing "defhalting eval-fn generation"
    (is (= (TEST-SELECTOR-eval-fn nil {:term {:A 1 :B [2] :C {:k 3} } }) [1 [2] {:k 3}])))
  (testing "defhalting record property names are correct"
    (let [{{a :A b :B c :C} :term} (descriptor/dtor  TEST-SELECTOR 1 [2] {:k 3})]
      (is (= a 1))
      (is (= b [2]))
      (is (= c {:k 3}))))
  (testing "defhalting can destructure maps"
    (defhalting overview-items [g {:keys [a b] :as args}
                                 C]
      (+ a b C))
    (is (= (overview-items-eval-fn :graph {:term {:args {:a 1 :b 1}
                                                   :C    1}})
           3))
    (let [{{{a :a b :b} :args
            C           :C} :term} (descriptor/dtor  overview-items {:a 1 :b 2} 3)]
      (is (= a 1))
      (is (= b 2))
      (is (= C 3)))))
