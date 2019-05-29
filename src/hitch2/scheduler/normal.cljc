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
       (-send-message! [process {:keys [gm gv effects]}]
         (run! (fn [effect] (g/run-effect gm effect)) effects))
       (-kill-process! [process]
         true)))
   :cljs
   (def default-dispatch-process
     (reify
       pm/IProcess
       (-send-message! [process {:keys [gm gv effects]}]
         (goog.async.run
           (fn []
             (run! (fn [effect] (g/run-effect gm effect)) effects))))
       (-kill-process! [process]
         true))))

(def eager-default-dispatch-process
   (reify
     pm/IProcess
     (-send-message! [process {:keys [gm gv effects]}]
       (run! (fn [effect] (g/run-effect gm (assoc effect :graph-value gv))) effects))
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
