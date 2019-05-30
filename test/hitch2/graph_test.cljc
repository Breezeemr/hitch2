(ns hitch2.graph-test
  (:require #?(:cljs [cljs.test :refer-macros [deftest is testing]]
               :clj [clojure.test :refer [deftest is testing]])
    [hitch2.curator.mutable-var :refer  [mutable-var]]
    [hitch2.graph :as h :refer [pin unpin]]
    [hitch2.graph-manager.atom :as atom-gm]
    [hitch2.test-common :refer [Constant]
     :as common]
    [hitch2.descriptor-impl-registry :as reg
     :refer [registry-resolver]]))

(def gctors
  [["Atom graph: " (fn [] (atom-gm/make-gm registry-resolver #_common/sync-scheduler))]])

(def mvdtor (mutable-var :mvdtor))

(doseq [[gname gctor] gctors]
  (deftest hook-unshared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (h/hook-sel g #(vswap! results conj %) (Constant 0))

      (is (= @results [0])
        (str gname "Unshared hook's cb sees resolved value immediately"))

      (pin g mvdtor)

      (is (= @results [0])
        (str gname "Unshared hook's cb not called if descriptor gains another ext-child"))))


  (deftest hook-unshared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (h/hook-sel g #(vswap! results conj %) mvdtor)

      (is (= @results [])
        (str gname "Unshared hook's cb uncalled before descriptor resolved."))

      (h/apply-commands g [[mvdtor [:set-value 0]]])

      (is (= @results [0])
        (str gname "Unshared hook's cb sees resolved value immediately"))

      (h/apply-commands g [[mvdtor [:set-value 1]]])

      (is (= @results [0])
        (str gname "Hooks do not get called more than once"))

      (pin g mvdtor)

      (is (= @results [0])
        (str gname "Unshared hook's cb not called if descriptor gains another ext-child"))))

  (deftest hook-shared-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (pin g (Constant 0))

      (h/hook-sel g #(vswap! results conj %) (Constant 0))

      (is (= @results [0])
        (str gname "Shared hook's cb sees resolved value immediately"))

      (pin g mvdtor)

      (is (= @results [0])
        (str gname "Shared hook's cb not called if descriptor gains another ext-child"))))


  (deftest hook-shared-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])]

      (pin g mvdtor)

      (h/hook-sel g #(vswap! results conj %) mvdtor)

      (is (= @results [])
        (str gname "Shared hook's cb uncalled before descriptor resolved."))

      (h/apply-commands g [[mvdtor [:set-value 0]]])

      (is (= @results [0])
        (str gname "Shared hook's cb sees resolved value immediately"))

      (h/apply-commands g [[mvdtor [:set-value 1]]])

      (is (= @results [0])
        (str gname "Hooks do not get called more than once"))

      (unpin g mvdtor)

      (is (= @results [0])
        (str gname "Shared hook's cb not called if descriptor loses another ext-child"))))


  (deftest hook-change-delayed-resolution
    (let [g       (gctor)
          results (volatile! [])
          unhook  (h/hook-change-sel g #(vswap! results conj %) mvdtor)]

      (is (= @results [])
        (str gname "Hook-change's cb should not see initial unresolved value"))

      (h/apply-commands g [[mvdtor [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should see unresolved->resolved value"))

      (h/apply-commands g [[mvdtor [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value didn't change"))

      (h/apply-commands g [[mvdtor [:clear]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value becomes unresolved."))

      (pin g mvdtor)

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if descriptor gains an ext-child"))

      (h/apply-commands g [[mvdtor [:set-value 0]]])

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if the value moves from X to unresolved then back to X."))

      (vreset! results [0])
      (unpin g mvdtor)

      (is (= @results [0])
        (str gname "Hook-change's cb should not be called again if descriptor loses an ext-child"))


      (h/apply-commands g [[mvdtor [:set-value 1]]])

      (is (= @results [0 1])
        (str gname "Hook-change's cb should be called when value changes."))

      (h/apply-commands g [[mvdtor [:set-value 2]]])

      (is (= @results [0 1 2])
        (str gname "Hook-change's cb should be called when value changes again."))

      (h/apply-commands g [[mvdtor [:set-value 1]]])

      (is (= @results [0 1 2 1])
        (str gname "Hook-change's cb should be called when value changes again, even if to a previously-seen value."))

      (pin g mvdtor)

      (unhook)

      (h/apply-commands g [[mvdtor [:set-value 3]]])

      (is (= @results [0 1 2 1])
        (str gname "Hook-change's cb should not be called after unhook"))))

  (deftest hook-change-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])
          _       (pin g mvdtor)
          _       (h/apply-commands g [[mvdtor [:set-value 0]]])
          unhook  (h/hook-change-sel g #(vswap! results conj %) mvdtor)]

      (is (= @results [0])
        (str gname "Hook-change's cb should see initial resolved value"))

      (h/apply-commands g [[mvdtor [:set-value 1]]])

      (is (= @results [0 1])
        (str gname "Hook-change's cb should see changed value"))

      (unhook)

      (h/apply-commands g [[mvdtor [:set-value 2]]])

      (is (= @results [0 1])
          (str gname "Hook-change's cb should not be called after unhook"))))

  (deftest hook-callback-test-immediate-resolution
    (let [g       (gctor)
          results (volatile! [])
          cb-dtor  (mutable-var :callback-dtor)
          _       (pin g cb-dtor)
          unhook  (h/hitch-callback g
                                    #(vswap! results conj %)
                                    (fn [rtx] @(h/select-sel! rtx cb-dtor)))]

      (is (= @results [])
          (str gname "Hook-callback's cb should see initial resolved value"))

      (h/apply-commands g [[cb-dtor [:set-value 1]]])

      (is (= @results [1])
          (str gname "Hook-callback's cb should see changed value")))))

