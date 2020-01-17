(ns hitch2.test-common
  #?(:clj (:import (java.io Writer)))
  (:require [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.protocols.graph-manager :as g]
            [hitch2.descriptor-impl-registry :as reg]
            [hitch2.scheduler.normal :as sched]))

(defn return-constant [ {[v] :term}]
  (fn [gv-tracker]
    v))
(def-descriptor-spec constant-spec
  :not-curator
  :hitch2.descriptor.spec/canonical-form  :hitch2.descriptor.spec.canonical-form/vector
  :hitch2.descriptor.spec/positional-params [:v])
(def constant-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/halting
   :hitch2.descriptor.impl/halting return-constant
   :hitch2.descriptor.impl/halting-slot-descriptor (fn [_dt _sel v] v)}

  #_(reify
                     descriptor-proto/ImplementationKind
                     (-imp-kind [curator] )
                     descriptor-proto/HaltingImplementation
                     (-get-halting-fn [sel]
                       return-constant)))

(reg/def-registered-descriptor constant-spec' constant-spec constant-impl)
(defn Constant [v]
  (descriptor/positional-dtor  constant-spec' v))
