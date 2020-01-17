(ns hitch2.curator.hook
  (:require [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
    ;[hitch2.scheduler.normal :refer [mmd-dtor]]
            [hitch2.descriptor :as descriptor]
            [hitch2.descriptor-impl-registry :as reg]
            [hitch2.process-manager :as pm]
            [hitch2.protocols.graph-manager :as g]))

(def hook-spec
  {:hitch2.descriptor/name ::hook
   :hitch2.descriptor.spec/kind :curator})

(def initial-node (assoc curator-proto/initial-curator-state :state {}))

(defn remove-called-hooks [state descriptors]
  (reduce dissoc state descriptors))

(def-descriptor-spec hook-manager-proc-spec
  :process)

(def-descriptor-spec hook-curator-spec
  :curator)

(def hook-manager-proc-dtor (descriptor/->dtor hook-manager-proc-spec nil))

(def hook-impl
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/curator
   ::curator-proto/init
   (fn [curator-descriptor] initial-node)
   ::curator-proto/observed-value-changes
   (fn [curator-descriptor]
     (fn [graph-value node parent-descriptors]
       (let [descriptor->targets (:state node)]
         (-> node
             (update-in [:outbox hook-manager-proc-dtor]
                        (fnil into [])
                        (mapcat (fn [descriptor]
                                  (for [target (seq (descriptor->targets descriptor))]
                                    {:type       :hook-call
                                     :target     target
                                     :descriptor descriptor})))
                        parent-descriptors)
             (update :state remove-called-hooks parent-descriptors)))))
   ::curator-proto/apply-command
   (fn [curator-descriptor]
     (fn [graph-value node command]
       (case (nth command 0)
         :hook-subscribe
         (let [[_ descriptor target] command]
           (-> node
               (update-in [:state descriptor] (fnil conj #{}) target)
               (update :change-focus assoc descriptor true)))
         :hook-unsubscribe
         (let [[_ descriptor target] command]
           (let [new-node (update-in node [:state descriptor] (fnil disj #{}) target)]
             (if (not-empty (get-in new-node [:state descriptor]))
               new-node
               (update new-node :change-focus assoc descriptor false)))))))})

(reg/def-registered-descriptor hook-curator-spec' hook-curator-spec hook-impl)

(def hook-curator
  (descriptor/->dtor  hook-curator-spec' nil))


(def-descriptor-spec hook-change-curator-spec
  :curator)

(def hook-change-impl
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/curator
   ::curator-proto/init
   (fn [curator-descriptor] initial-node)
   ::curator-proto/observed-value-changes
   (fn [curator-descriptor]
     (fn [graph-value node parent-descriptors]
       (let [descriptor->targets (:state node)]
         (-> node
             (update-in [:outbox hook-manager-proc-dtor]
                        (fnil into [])
                        (mapcat (fn [descriptor]
                                  (for [target (seq (descriptor->targets descriptor))]
                                    {:type       :hook-changes-call
                                     :target     target
                                     :descriptor descriptor})))
                        parent-descriptors)))))
   ::curator-proto/apply-command
   (fn [curator-descriptor]
     (fn [gmv node command]
       (let [graph-value (graph-proto/-graph-value gmv)]
         (case (nth command 0)
           :hook-change-subscribe
           (let [[_ descriptor target] command
                 current-descriptor-value (get graph-value descriptor NOT-FOUND-SENTINEL)]
             (cond->
               (-> node
                   (update-in [:state descriptor] (fnil conj #{}) target)
                   (update :change-focus assoc descriptor true))
               (not (identical? current-descriptor-value NOT-FOUND-SENTINEL))
               (update-in [:outbox hook-manager-proc-dtor]
                          (fnil conj [])
                          {:type       :hook-changes-call
                           :target     target
                           :descriptor descriptor})))
           :hook-change-unsubscribe
           (let [[_ descriptor target] command]
             (let [new-node (update-in node [:state descriptor] disj target)]
               (if (not-empty (get-in new-node [:state descriptor]))
                 new-node
                 (update new-node :change-focus assoc descriptor false)))))))
     )})

(reg/def-registered-descriptor hook-change-curator-spec' hook-change-curator-spec hook-change-impl)


(def hook-change-curator
  (descriptor/->dtor  hook-change-curator-spec' nil))

(def hook-manager-proc-spec-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/process
   ::pm/create
   (fn [pdtor]
     (reify
       pm/IProcess
       (-send-message! [process {:keys [graph-value
                                        target
                                        descriptor]
                                 :as   effect}]
         (let [v (get graph-value descriptor)]
           (target v)))
       (-kill-process! [process]
         true)))})

(reg/def-registered-descriptor hook-manager-proc-spec' hook-manager-proc-spec hook-manager-proc-spec-impl)

