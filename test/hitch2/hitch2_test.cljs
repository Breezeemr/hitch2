(ns hitch2.hitch2-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [hitch2.hitch2 :refer [multiply]]))

(deftest multiply-test
  (is (= (* 1 2) (multiply 1 2))))

(deftest multiply-test-2
  (is (= (* 75 10) (multiply 10 75))))
