(ns hitch2.graph-test
  (:require #?(:cljs [cljs.test :refer [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
    [hitch2.machine.mutable-var :refer  [mutable-var]]
    [hitch2.graph :as h :refer [pin unpin]]
    [hitch2.graph-manager.atom :as atom-gm]
    [hitch2.test-common :refer [Constant]]))


(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm))]])

(def mvsel (mutable-var :mvsel))

(doseq [[gname gctor] gctors]
  (deftest hook-unshared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (h/hook-sel g #(vswap! results conj %) (Constant 0))

      (is (= @results [0])
        (str gname "Unshared hook's cb sees resolved value immediately"))

      (pin g mvsel)

      (is (= @results [0])
        (str gname "Unshared hook's cb not called if selector gains another ext-child"))))


  ;(deftest hook-unshared-delayed-resolution
  ;  (let [g       (gctor)
  ;        results (volatile! [])]
  ;
  ;    (h/hook-sel g #(vswap! results conj %) mvsel)
  ;
  ;    (is (= @results [])
  ;      (str gname "Unshared hook's cb uncalled before selector resolved."))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 0]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Unshared hook's cb sees resolved value immediately"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 1]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hooks do not get called more than once"))
  ;
  ;    (pin g mvsel)
  ;
  ;    (is (= @results [0])
  ;      (str gname "Unshared hook's cb not called if selector gains another ext-child"))))
  ;
  ;(deftest hook-shared-immediate-resolution
  ;  (let [g       (gctor)
  ;        results (volatile! [])]
  ;
  ;    (pin g (Constant 0))
  ;
  ;    (h/hook-sel g #(vswap! results conj %) (Constant 0))
  ;
  ;    (is (= @results [0])
  ;      (str gname "Shared hook's cb sees resolved value immediately"))
  ;
  ;    (pin g mvsel)
  ;
  ;    (is (= @results [0])
  ;      (str gname "Shared hook's cb not called if selector gains another ext-child"))))
  ;
  ;
  ;(deftest hook-shared-delayed-resolution
  ;  (let [g       (gctor)
  ;        results (volatile! [])]
  ;
  ;    (pin g mvsel)
  ;
  ;    (h/hook-sel g #(vswap! results conj %) mvsel)
  ;
  ;    (is (= @results [])
  ;      (str gname "Shared hook's cb uncalled before selector resolved."))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 0]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Shared hook's cb sees resolved value immediately"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 1]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hooks do not get called more than once"))
  ;
  ;    (unpin g mvsel)
  ;
  ;    (is (= @results [0])
  ;      (str gname "Shared hook's cb not called if selector loses another ext-child"))))
  ;
  ;
  ;(deftest hook-change-delayed-resolution
  ;  (let [g       (gctor)
  ;        results (volatile! [])
  ;        unhook  (h/hook-change-sel g #(vswap! results conj %) mvsel)]
  ;
  ;    (is (= @results [])
  ;      (str gname "Hook-change's cb should not see initial unresolved value"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 0]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should see unresolved->resolved value"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 0]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should not be called again if the value didn't change"))
  ;
  ;    (h/apply-commands g [[mvsel [:clear]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should not be called again if the value becomes unresolved."))
  ;
  ;    (pin g mvsel)
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should not be called again if selector gains an ext-child"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 0]]])
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should not be called again if the value moves from X to unresolved then back to X."))
  ;
  ;
  ;    (unpin g mvsel)
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should not be called again if selector loses an ext-child"))
  ;
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 1]]])
  ;
  ;    (is (= @results [0 1])
  ;      (str gname "Hook-change's cb should be called when value changes."))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 2]]])
  ;
  ;    (is (= @results [0 1 2])
  ;      (str gname "Hook-change's cb should be called when value changes again."))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 1]]])
  ;
  ;    (is (= @results [0 1 2 1])
  ;      (str gname "Hook-change's cb should be called when value changes again, even if to a previously-seen value."))
  ;
  ;    (pin g mvsel)
  ;
  ;    (unhook)
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 3]]])
  ;
  ;    (is (= @results [0 1 2 1])
  ;      (str gname "Hook-change's cb should not be called after unhook"))))
  ;
  ;(deftest hook-change-immediate-resolution
  ;  (let [g       (gctor)
  ;        results (volatile! [])
  ;        _       (pin g mvsel)
  ;        _       (h/apply-commands g [[mvsel [:set-value 0]]])
  ;        unhook  (h/hook-change-sel g #(vswap! results conj %) mvsel)]
  ;
  ;    (is (= @results [0])
  ;      (str gname "Hook-change's cb should see initial resolved value"))
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 1]]])
  ;
  ;    (is (= @results [0 1])
  ;      (str gname "Hook-change's cb should see changed value"))
  ;
  ;    (unhook)
  ;
  ;    (h/apply-commands g [[mvsel [:set-value 2]]])
  ;
  ;    (is (= @results [0 1])
  ;      (str gname "Hook-change's cb should not be called after unhook"))))
  )

