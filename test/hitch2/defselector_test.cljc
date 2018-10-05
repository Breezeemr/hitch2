(ns hitch2.defselector-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
            [hitch2.selector :as sel]
            [hitch2.protocols.selector :as sel-proto]))

(deftest selectors
  (testing "defselector definition does not error-out"
    (sel/defselector TEST-SELECTOR [G A [B1 B2 :as B] {C1 :k :as C}]
      [A B C]))
  (testing "defselector eval-fn generation"
    (is (= (TEST-SELECTOR-eval-fn nil 1 [2] {:k 3}) [1 [2] {:k 3}])))
  (testing "defselector record property names are correct"
    (let [[a b c] (sel-proto/sel TEST-SELECTOR 1 [2] {:k 3})]
      (is (= a 1))
      (is (= b [2]))
      (is (= c {:k 3})))))
