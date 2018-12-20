(ns hitch2.curator.hitch-callback
  (:require [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.halt :as halt]
            [hitch2.tx-manager.halting :as halting]
            [hitch2.protocols.tx-manager :as tx-manager]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor :as descriptor]
            [hitch2.descriptor-impl-registry :as reg]
            [clojure.set :as set])
  #?(:clj
     (:import [java.util UUID])))

(def initial-node
  (assoc curator-proto/initial-curator-state
         :state {:id->info      {} ;; info [halt-fn args callback]
                 :id->sels      {}
                 :sel->id       {}
                 :dirty-ids     #{}}))

(def-descriptor-spec react-hook-spec
  :curator)

(defn update-reverse-indexes
  [node id new-parents]
  (let [state               (:state node)
        id->sel             (:id->sel state)
        sel->id             (:sel->id state)
        old-parents         (get id->sel id #{})
        del-parents         (set/difference old-parents new-parents)
        add-parents         (set/difference new-parents old-parents)
        id->sel'            (assoc id->sel id new-parents)
        shared-parent-delta (volatile! {})
        sel->id'            (as-> sel->id <>
                              (reduce
                                (fn [sel->id sel]
                                  (when (nil? (get sel->id sel))
                                    (vswap! shared-parent-delta assoc sel true))
                                  (update sel->id sel (fnil conj #{}) id))
                                <> add-parents)
                              (reduce
                                (fn [sel->id sel]
                                  (let [ids        (get sel->id sel #{})
                                        ids'       (disj ids id)
                                        went-away? (and (pos? (count ids)) (zero? (count ids')))]
                                    (when went-away?
                                      (vswap! shared-parent-delta assoc sel false))
                                    (if went-away?
                                      (dissoc sel->id sel)
                                      (assoc sel->id sel ids'))))
                                <> del-parents))
        state'              (assoc state :id->sel id->sel'
                                   :sel->id sel->id')]
    (-> node
        (assoc :state state')
        (update :change-focus into @shared-parent-delta))))

(def react-hook-impl
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/curator

   ::curator-proto/init
   (fn [curator-descriptor] initial-node)

   ::curator-proto/apply-command
   (fn [curator-descriptor graph-value node command]
     (case (nth command 0)
       :hitch-callback-reset-parents
       (let [[_ halt-fn cb new-parents] command
             id #?(:cljs (random-uuid)
                   :clj  (UUID/randomUUID))]
         (-> node
             (assoc-in [:state :id->info id] {:halt-fn  halt-fn
                                              :callback cb})
             (update-reverse-indexes id new-parents)))
       :hitch-callback-reset-parents-id
       (let [[_ id new-parents] command]
         (update-reverse-indexes node id new-parents))
       :hitch-callback-unsubscribe
       (let [[_ id] command]
         (-> node
             (update-in [:state :id->info] dissoc id)
             (update-reverse-indexes id #{})))))
   
   ::curator-proto/observed-value-changes
   (fn [curator-descriptor graph-value node parent-descriptors]
     (let [sel->id   (-> node :state :sel->id)
           dirty-id  (-> node :state :dirty-ids)
           dirty-id' (transduce
                       (map sel->id)
                       into dirty-id parent-descriptors)]
       (assoc-in node [:state :dirty-ids] dirty-id')))

   ::curator-proto/finalize
   (fn [_ graph-value node]
     (let [dirty-ids (-> node :state :dirty-ids)]
       (cond-> (assoc-in node [:state :dirty-ids] #{})
         (pos? (count dirty-ids))
         (update :async-effects conj
                 {:type  :hitch-callback-rerun-body
                  :infos (select-keys (-> node :state :id->info) dirty-ids)}))))})

(reg/def-registered-descriptor hitch-callback-curator react-hook-spec react-hook-impl)

(def hitch-callbacker (descriptor/->dtor hitch-callback-curator nil))

(defn first-run
  [gm halt-fn cb]
  (let [gv              (graph-proto/-get-graph gm)
        rtx             (halting/halting-manager gv)
        result          (halt/maybe-halt (halt-fn rtx) NOT-FOUND-SENTINEL)
        focus-descriptors (tx-manager/finish-tx! rtx)]
    (if (= result NOT-FOUND-SENTINEL)
      (graph-proto/-transact! gm hitch-callbacker
                              [:hitch-callback-reset-parents halt-fn cb focus-descriptors])
      (cb result))))

(defn name-later
  [gm id {:keys [callback halt-fn] :as info}]
  (let [gv              (graph-proto/-get-graph gm)
        rtx             (halting/halting-manager gv)
        result          (halt/maybe-halt (halt-fn rtx) NOT-FOUND-SENTINEL)
        focus-descriptors (tx-manager/finish-tx! rtx)]
    (if (= result NOT-FOUND-SENTINEL)
      (graph-proto/-transact! gm hitch-callbacker
                              [:hitch-callback-reset-parents-id id focus-descriptors])
      (do
        (graph-proto/-transact! gm hitch-callbacker
                                [:hitch-callback-unsubscribe id])
        (callback result)))))

(defmethod graph-proto/run-effect :hitch-callback-rerun-body
  [gm effect]
  (doseq [[id info] (:infos effect)]
    (name-later gm id info)))
