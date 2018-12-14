(ns hitch2.test-common
  #?(:clj (:import (java.io Writer)))
  (:require [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.protocols.graph-manager :as g]
            [hitch2.selector-impl-registry :as reg]))

(defn return-constant [gv-tracker {[v] :term}]
  v)
(def-descriptor-spec constant-spec
  :not-machine
  :hitch.selector.spec/canonical-form :hitch.selector.spec.canonical-form/positional
  :hitch.selector.spec/positional-params [:v])
(def constant-impl
  {:hitch2.descriptor.impl/kind :hitch.selector.kind/halting
   :hitch.selector.impl/halting return-constant
   :hitch.selector.impl/halting-slot-selector (fn [_dt _sel v] v)}

  #_(reify
                     selector-proto/ImplementationKind
                     (-imp-kind [machine] )
                     selector-proto/HaltingImplementation
                     (-get-halting-fn [sel]
                       return-constant)))

(reg/def-registered-selector constant-spec' constant-spec constant-impl)
(defn Constant [v]
  (descriptor/dtor  constant-spec' v))

(def sync-scheduler
  #?(:clj (reify g/IScheduler
            (-run-sync [_ gm effects]
              (run! (fn [effect] (g/run-effect gm effect)) effects))
            (-run-async [_ gm effects]
              (run! (fn [effect] (g/run-effect gm effect)) effects)))
     :cljs (reify g/IScheduler
             (-run-sync [_ gm effects]
               (run! (fn [effect] (g/run-effect gm effect)) effects))
             (-run-async [_ gm effects]
               (run! (fn [effect] (g/run-effect gm effect)) effects)))))
