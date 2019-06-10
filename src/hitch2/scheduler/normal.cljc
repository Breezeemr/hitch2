(ns hitch2.scheduler.normal
  (:require [hitch2.protocols.graph-manager :as g]
            [hitch2.process-manager :as pm]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor-impl-registry :as reg]
            [hitch2.descriptor :as descriptor])
  #?(:cljs (:import goog.async.run)))

(def-descriptor-spec multi-method-dispatch-spec
  :process)

(def-descriptor-spec async-multi-method-dispatch-spec
  :process)

(def async-mmd-dtor (descriptor/->dtor async-multi-method-dispatch-spec nil))

#?(:clj
   (def default-dispatch-process
     {:hitch2.descriptor.impl/kind
      :hitch2.descriptor.kind/process
      ::pm/create
      (fn [pdtor]
        (reify
          pm/IProcess
          (-send-message! [process {:keys [gm] :as effect}]
            (future
              (g/run-effect gm effect)))
          (-kill-process! [process]
            true)))})
   :cljs
   (def default-dispatch-process
     {:hitch2.descriptor.impl/kind
      :hitch2.descriptor.kind/process
      ::pm/create
      (fn [pdtor]
        (reify
          pm/IProcess
          (-send-message! [process {:keys [gm] :as effect}]
            (goog.async.run
              (fn []
                (g/run-effect gm effect))))
          (-kill-process! [process]
            true)))}))

(def eager-default-dispatch-process
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/process
   ::pm/create
   (fn [pdtor]
     (reify
       pm/IProcess
       (-send-message! [process {:keys [gm] :as effect}]
         (g/run-effect gm effect))
       (-kill-process! [process]
         true)))})


(reg/def-registered-descriptor async-multi-method-dispatch-spec' async-multi-method-dispatch-spec default-dispatch-process)
(reg/def-registered-descriptor multi-method-dispatch-spec' multi-method-dispatch-spec eager-default-dispatch-process)

(defn default-process-manager [resolver]
  (pm/->pm resolver))
