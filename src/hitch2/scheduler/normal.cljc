(ns hitch2.scheduler.normal
  (:require [hitch2.protocols.graph-manager :as g]
            [hitch2.process-manager :as pm]
            [hitch2.descriptor :as descriptor])
  #?(:cljs (:import goog.async.run)))

(def multi-method-dispatch-spec
  {:hitch2.descriptor/name 'hitch2.scheduler.normal/multi-method-dispatch})

(def mmd-dtor (descriptor/->dtor multi-method-dispatch-spec nil))

#?(:clj
   (def default-dispatch-process
     (reify
       pm/IProcess
       (-send-message! [process {:keys [gm] :as effect}]
         (g/run-effect gm effect))
       (-kill-process! [process]
         true)))
   :cljs
   (def default-dispatch-process
     (reify
       pm/IProcess
       (-send-message! [process {:keys [gm] :as effect}]
         (goog.async.run
           (fn []
             (g/run-effect gm effect))))
       (-kill-process! [process]
         true))))

(def eager-default-dispatch-process
   (reify
     pm/IProcess
     (-send-message! [process {:keys [gm] :as effect}]
       (g/run-effect gm effect))
     (-kill-process! [process]
       true)))

(defn pm-factory [ps-dtor]
  ({mmd-dtor default-dispatch-process} ps-dtor))

(def default-process-manager
  (pm/->pm pm-factory))

(defn eager-factory [ps-dtor]
  ({mmd-dtor eager-default-dispatch-process} ps-dtor))

(def eager-process-manager
  (pm/->pm eager-factory))
