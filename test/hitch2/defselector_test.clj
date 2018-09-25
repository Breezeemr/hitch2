(ns hitch2.defselector-test
  (:require [clojure.test :refer [is deftest testing]]
            [hitch2.selector :as sel]))

(deftest selectors
  (testing "defselector definition does not error-out"
    (sel/defselector TEST-SELECTOR [G A [B1 B2 :as B] {C1 :k :as C}]
      [A B C]))
  (testing "defselector eval-fn generation"
    (is (= (TEST-SELECTOR-eval-fn nil 1 [2] {:k 3}) [1 [2] {:k 3}])))
  (testing "defselector record property names are correct"
    (let [{:keys [a b c]} (TEST-SELECTOR 1 [2] {:k 3})]
      (is (= a 1))
      (is (= b [2]))
      (is (= c {:k 3})))))
