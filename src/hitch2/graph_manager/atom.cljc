(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.def.curator :as curator-proto]
             [hitch2.descriptor :as descriptor]
             [hitch2.protocols.tx-manager :as tx-manager-proto]
             [hitch2.tx-manager.halting :as halting-tx]
             [hitch2.halt :as halt])
  #?(:cljs (:import goog.async.run)))


(defrecord GraphManagerValue [graph-value
                              node-state
                              observes
                              observed-by])
(defrecord curator-state [node])
(defrecord deriving-state [change-focus waiting value-changed?])
(defrecord var-state [value-changed?])

(def #?(:cljs    ^:dynamic ^boolean *trace*
        :default ^:dynamic *trace*)
  "Whether the ImmutableGraph will include a key ::trace in the command-result,
  which is a list of internal ops executed during the transaction. Default false."
  false)

(defonce ^:private op-history (volatile! []))

(defn- record! [op]
  (vswap! op-history conj op)
  nil)

(defn get-trace [] @op-history)
(defn clear-trace! [] (vreset! op-history []))

(defn get-curator [impl dtor]
  (if-some [f (:hitch2.descriptor.impl/get-curator impl)]
    (f dtor)
    (assert false)))

(s/def ::descriptor any?)
(s/def ::graph-value (s/map-of ::descriptor any?))
(s/def ::derivation-state any?)
(s/def ::node-state (s/map-of
                      ::descriptor
                      (s/or
                        :curator-state
                        ::curator-proto/curator-state
                        :derivation-state
                        ::derivation-state)))

(s/def ::observes (s/map-of ::descriptor (s/coll-of ::descriptor)))
(s/def ::observed-by (s/map-of ::descriptor (s/coll-of ::descriptor)))

(s/def ::graph-manager-value
  (s/keys
    :req-un [::graph-value
             ::node-state
             ::observes
             ::observed-by]))

(defn get-observes [graph-manager-value descriptor]
  (-> graph-manager-value :observes (get descriptor)))

(defn get-observed-by [graph-manager-value descriptor]
  (-> graph-manager-value :observed-by (get descriptor)))

(defn get-node-state [graph-manager-value descriptor]
  (-> graph-manager-value :node-state (get descriptor)))

(defn get-graph-value [graph-manager-value]
  (-> graph-manager-value :graph-value))


(defn- add-to-working-set [working-set descriptor]
  (vswap! working-set conj! descriptor)
  nil)

(defn tx-init-curator [node-state graph-manager-value resolver descriptor disturbed-curators]
  (if (contains? @disturbed-curators descriptor)
    node-state
    (do
      (add-to-working-set disturbed-curators descriptor)
      (if-some [tx-init (::curator-proto/tx-init (resolver descriptor))]
        (tx-init descriptor (get-graph-value graph-manager-value) node-state)
        node-state))))

(declare propagate-dependency-changes)

(defn get-init-node [graph-manager-value resolver descriptor disturbed-curators]
  (if-some [node-state (get-node-state graph-manager-value descriptor)]
    node-state
    (let [dtor-impl (resolver descriptor)
          dtor-kind (:hitch2.descriptor.impl/kind dtor-impl)]
      (case dtor-kind
        :hitch2.descriptor.kind/curator
        (if-some [init (::curator-proto/init (resolver descriptor))]
          (init descriptor)
          curator-proto/initial-curator-state)
        :hitch2.descriptor.kind/var
        (->var-state false)
        :hitch2.descriptor.kind/halting
        (->deriving-state
          [] #{} false)
        ))))

(defn update-graph-value [gv dtor value]
  (if (identical? value NOT-FOUND-SENTINEL)
    (dissoc gv dtor)
    (assoc gv dtor value)))

(defn propagate-set-projections [graph-manager-value set-projections worklist-atom]
  (reduce-kv (fn [gv dtor value]
               (let [old-value (-> gv :graph-value (get dtor NOT-FOUND-SENTINEL))]
                 (if (= old-value value)
                   gv
                   (do
                     (when-not (identical? value NOT-FOUND-SENTINEL)
                       (add-to-working-set worklist-atom dtor))
                     (-> gv
                         (assoc-in [:node-state dtor :value-changed?]
                           true #_(if (identical? value NOT-FOUND-SENTINEL)
                                                                       false
                                                                       true))
                         (update :graph-value update-graph-value dtor value))))))
    graph-manager-value
    set-projections))

(defn halting [descriptor simpl tx-manager]
  (halt/maybe-halt
    ((:hitch2.descriptor.impl/halting simpl) tx-manager
      descriptor)
    NOT-FOUND-SENTINEL))
;todo partial evaluate the destructuring and return an clojure that takes a graph.

(defn run-halting [graph-manager-value
                   node-state
                   resolver
                   descriptor
                   simpl
                   worklist-atom
                   dirty-curators]
  (let [old-value (-> graph-manager-value :graph-value (get descriptor NOT-FOUND-SENTINEL))
        old-deps  (-> graph-manager-value :observes (get descriptor #{}))
        tx-manager (halting-tx/halting-manager (:graph-value graph-manager-value))
        ;;; NOTE: change this line to switch halting implementations
        new-value (halting descriptor simpl tx-manager)
        deps (tx-manager-proto/finish-tx! tx-manager)
        value-changed? (and (not= new-value old-value) (not (identical? new-value NOT-FOUND-SENTINEL)))
        added-deps       (into #{} (remove old-deps) deps)
        waiting-deps   (into #{} (remove (:graph-value graph-manager-value)) deps)
        change-focus (-> {}
                           (into (map (fn [dep]
                                        [dep true]))
                             added-deps)
                           (into (comp (remove deps)
                                   (map (fn [dep]
                                          [dep false])))
                                 old-deps))]
    (when value-changed?
      (add-to-working-set worklist-atom descriptor))
    (cond-> (assoc-in
              graph-manager-value
              [:node-state descriptor]
              (cond-> node-state
                value-changed?
                (assoc
                  :value-changed?
                  true)
                (not-empty waiting-deps)
                (assoc
                  :waiting
                  waiting-deps)))
      :always
      (update
        :graph-value
        update-graph-value descriptor new-value)
      (not-empty change-focus)
      (propagate-dependency-changes resolver descriptor change-focus worklist-atom dirty-curators))))


(defn addval [x k v]
  (if-some [a (get x k)]
    (assoc x k (conj a v))
    (assoc x k (conj #{} v))))

(defn remove-val [x k v]
  (if-some [a (get x k)]
    (if-some [nv (not-empty (disj a v))]
      (assoc x k nv)
      (dissoc x k))
    x))

(defn propagate-value-changes [graph-manager-value resolver parent worklist-atom dirty-curators]
  (reduce
    (fn [graph-manager-value descriptor]
      (let [dtor-impl (resolver descriptor)
            dtor-kind (:hitch2.descriptor.impl/kind dtor-impl)
            node-state (get-node-state graph-manager-value descriptor)]
        (assert node-state)
        (case dtor-kind
          :hitch2.descriptor.kind/curator
          (let [graph-value    (get-graph-value graph-manager-value)
                {:keys [sync-effects async-effects]
                 new-set-projections     :set-projections
                 new-change-focus :change-focus
                 :as                new-node-state}
                (if-some [observed-value-changes (::curator-proto/observed-value-changes dtor-impl)]
                  (observed-value-changes descriptor graph-value
                    (tx-init-curator node-state graph-manager-value  resolver  descriptor dirty-curators)
                    #{parent})
                  (assert false))]
            (s/assert ::curator-proto/curator-state new-node-state)
            (when (or (not-empty new-set-projections)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (add-to-working-set worklist-atom descriptor))
            (cond-> (assoc-in graph-manager-value
                      [:node-state descriptor]
                      new-node-state)
              (not-empty new-change-focus)
              (->
                (assoc-in
                  [:node-state descriptor :change-focus] {})
                (propagate-dependency-changes resolver descriptor new-change-focus worklist-atom dirty-curators))))
          ;:hitch2.descriptor.kind/var
          ;(assert false "should not happen")
          :hitch2.descriptor.kind/halting
          (let [{:keys [waiting] :as node-state}
                (-> graph-manager-value
                    :node-state
                    (get descriptor NOT-FOUND-SENTINEL)
                    (update :waiting disj parent))]
            (assert (not= node-state NOT-FOUND-SENTINEL))
            (if (empty? waiting)
              (run-halting
                graph-manager-value
                node-state
                resolver
                descriptor
                dtor-impl
                worklist-atom
                dirty-curators)
              (assoc-in graph-manager-value [:node-state descriptor] node-state))))))
    graph-manager-value
    (-> graph-manager-value :observed-by (get parent))))


(defn propagate-node-changes [resolver worklist-atom dirty-curators]
  (fn [graph-manager-value descriptor]
    (let [dtor-impl (resolver descriptor)
          dtor-kind (:hitch2.descriptor.impl/kind dtor-impl)]
      (if-some [node-state (get-node-state graph-manager-value descriptor)]
        (case dtor-kind
          :hitch2.descriptor.kind/curator
          (let [{:keys [change-focus set-projections]}
                node-state]
            (s/assert ::curator-proto/curator-state node-state)
            (when (not-empty set-projections)
              (add-to-working-set worklist-atom descriptor))
            (when *trace* (record! [:node-changes :curator (:name dtor-impl)
                                    descriptor
                                    node-state]))
            (cond-> graph-manager-value
              (not-empty change-focus)
              (->
                (update-in [:node-state descriptor]
                  assoc
                  :change-focus {})
                (propagate-dependency-changes resolver descriptor change-focus worklist-atom dirty-curators))
              (not-empty set-projections)
              (->
                (update-in [:node-state descriptor]
                  assoc
                  :set-projections {})
                (propagate-set-projections set-projections worklist-atom))))
          :hitch2.descriptor.kind/var
          (let [{:keys [value-changed?]}
                node-state]
            (when *trace* (record! [:node-changes :var (:name dtor-impl)
                                    descriptor]))
            (cond-> graph-manager-value
              value-changed?
              (->
                (update-in [:node-state descriptor]
                  assoc
                  :value-changed? false)
                (propagate-value-changes resolver descriptor worklist-atom dirty-curators))))
          :hitch2.descriptor.kind/halting
          (let [{:keys [value-changed? change-focus]}
                node-state]
            (when *trace*
              (record! [:node-changes :halting (:name dtor-impl)
                        descriptor (-> graph-manager-value :graph-value (get descriptor))])
              )
            (cond-> graph-manager-value
              (not-empty change-focus)
              (->
                (update-in [:node-state descriptor]
                  assoc
                  :change-focus {})
                (propagate-dependency-changes resolver descriptor change-focus worklist-atom dirty-curators))
              value-changed?
              (->
                (update-in [:node-state descriptor]
                  assoc
                  :value-changed? false)
                (propagate-value-changes resolver descriptor worklist-atom dirty-curators)))))
        (do                                                 ; (prn descriptor "on changelist after removed")
          graph-manager-value)))))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::descriptors))
  :ret ::graph-manager-value)

(defn flush-tx [node-state graph-manager-value resolver  descriptor]
  (if-some [flush-tx (::curator-proto/flush-tx  (resolver descriptor))]
    (flush-tx descriptor (:graph-value graph-manager-value) node-state)
    node-state))

(defn flush-worklist [graph-manager-value resolver dirty-curators-snapshot flush-worklist-atom]
  (reduce
    (fn [graph-manager-value curator]
      (let [old-node-state (get-node-state graph-manager-value curator)
            new-node-sate  (flush-tx old-node-state graph-manager-value resolver curator)]
        (assert old-node-state)
        (if (= old-node-state new-node-sate)
          graph-manager-value
          (do (add-to-working-set flush-worklist-atom curator)
            (assoc-in graph-manager-value [:node-state curator] new-node-sate)))))
    graph-manager-value
    dirty-curators-snapshot))

(defn propagate-changes [graph-manager-value resolver work-list dirty-curators recursion-limit]
  (let [new-work-list-atom (volatile! (transient #{}))
        graph-manager-value (reduce
                                  (propagate-node-changes
                                    resolver
                                    new-work-list-atom
                                    dirty-curators)
                                  graph-manager-value
                                  work-list)]
    (assert (not (zero? recursion-limit)))
    (if-some [new-work-list (not-empty (persistent! @new-work-list-atom))]
      (recur graph-manager-value resolver new-work-list dirty-curators (dec recursion-limit))
      (let [dirty-curators-snapshot (persistent! @dirty-curators)
            _ (vreset! dirty-curators (transient dirty-curators-snapshot))
            flush-worklist-atom (volatile! (transient #{}))
            graph-manager-value (flush-worklist graph-manager-value resolver dirty-curators-snapshot flush-worklist-atom)]
        (if-some [flush-worklist (not-empty (persistent! @flush-worklist-atom))]
          (recur graph-manager-value resolver flush-worklist dirty-curators (dec recursion-limit))
          graph-manager-value)))))

(defn apply-child-change-commands [graph-manager-value resolver child changes worklist-atom dirty-curators]
  (reduce-kv
    (fn [graph-manager-value parent added|removed]
      (let [dtor-impl   (resolver parent)
            dtor-kind   (:hitch2.descriptor.impl/kind dtor-impl)
            node-state (get-init-node graph-manager-value resolver parent dirty-curators)]
        (case dtor-kind
          :hitch2.descriptor.kind/curator
          (let [graph-value        (get-graph-value graph-manager-value)
                {:keys [sync-effects async-effects]
                 new-change-focus :change-focus
                 set-projections         :set-projections
                 :as                new-node-state}
                (if-some [curation-changes (::curator-proto/curation-changes dtor-impl)]
                  (curation-changes parent graph-value
                    (tx-init-curator node-state graph-manager-value  resolver parent dirty-curators)
                    (when added|removed
                      #{child})
                    (when (not added|removed)
                      #{child}))
                  (assert false))]
            (s/assert ::curator-proto/curator-state new-node-state)
            (when (or (not-empty set-projections)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (add-to-working-set worklist-atom parent))
            (when *trace*
              (record! [:child-change :curator
                        (:name dtor-impl)]))
            (let [new-graph-manager-value
                  (cond->
                    (assoc-in graph-manager-value
                      [:node-state parent]
                      (assoc new-node-state
                        :change-focus {}))
                    (not-empty new-change-focus)
                    (propagate-dependency-changes resolver parent new-change-focus worklist-atom dirty-curators))]
              (case added|removed
                true new-graph-manager-value
                false (-> ;deinit lifecycle
                        (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by parent]))]
                          new-graph-manager-value
                          new-graph-manager-value)))))
          :hitch2.descriptor.kind/var
          (let [curator (get-curator dtor-impl parent)]
            (assert (descriptor/descriptor? curator) (pr-str parent))
            (when *trace*
              (record! [:child-change :var
                        (:name dtor-impl)]))
            (case added|removed
              true (if (get-in graph-manager-value [:observes parent curator])
                     graph-manager-value
                     (propagate-dependency-changes graph-manager-value resolver parent {curator true} worklist-atom dirty-curators))
              false (->                                     ;deinit
                      (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by parent]))]
                        graph-manager-value
                        (-> graph-manager-value
                            (update :graph-value dissoc parent)
                            (propagate-dependency-changes resolver parent {curator false} worklist-atom dirty-curators)))
                      (update :node-state dissoc parent))))
          :hitch2.descriptor.kind/halting
          (let [old-node-state (get-in graph-manager-value [:node-state parent] NOT-FOUND-SENTINEL)]
            (when *trace*
              (record! [:child-change :halting
                        (:name dtor-impl)
                        child
                        parent]))
            (case added|removed
              true (if (identical? old-node-state NOT-FOUND-SENTINEL)
                     (run-halting
                       graph-manager-value
                       node-state
                       resolver
                       parent
                       dtor-impl
                       worklist-atom
                       dirty-curators)
                     graph-manager-value)
              false (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by parent]))]
                      graph-manager-value
                      (->                                   ;deinit
                        (if-some [observes (not-empty (get-in graph-manager-value [:observes parent]))]
                          (-> graph-manager-value
                              (update :graph-value dissoc parent)
                              (propagate-dependency-changes
                                resolver
                                parent
                                (into {}
                                  (map (fn [x]
                                         [x false]))
                                  observes)
                                worklist-atom
                                dirty-curators))
                          graph-manager-value)
                        (update :node-state dissoc parent))))))))
    graph-manager-value
    changes))

(defn update-observed-by [observed-by descriptor changes]
  (reduce-kv
    (fn [acc focus add|remove]
      (if add|remove
        (addval acc focus descriptor)
        (remove-val acc focus descriptor)))
    observed-by
    changes))

(defn update-observes [observes descriptor changes]
  (reduce-kv
    (fn [acc focus add|remove]
      (if add|remove
        (addval acc descriptor focus)
        (remove-val acc descriptor focus)))
    observes
    changes)
  )

(defn propagate-dependency-changes [graph-manager-value resolver descriptor changes worklist-atom dirty-curators]
  (apply-child-change-commands
    (-> graph-manager-value
        (update :observed-by update-observed-by descriptor changes)
        (update :observes update-observes descriptor changes))
    resolver
    descriptor
    changes
    worklist-atom
    dirty-curators))

(s/fdef -apply-command
  :args (s/cat
          :curator-state  ::curator-proto/curator-state
          :curator any?
          :command vector?
          :graph-value ::graph-value
          :observed-by (s/coll-of ::descriptor)
          :observes (s/coll-of ::descriptor))
  :ret ::curator-proto/curator-state)

(defn remove-effects [node-state curators]
  (reduce
    (fn [acc curator]
      (update acc curator assoc :sync-effects [] :async-effects []))
    node-state
    curators))

(defn finalize-tx [node-state graph-value resolver descriptor]
  (if-some [finalize (::curator-proto/finalize (resolver descriptor))]
    (finalize descriptor graph-value node-state)
    node-state))

(defn assert-valid-finalized-node-state [{:keys [change-focus set-projections]} descriptor-name]
  (assert (empty? change-focus) descriptor-name)
  (assert (empty? set-projections) descriptor-name))

(defn into! [target source]
  (reduce
    conj!
    target
    source))

(defn finalize-effects
  [graph-manager-value resolver disturbed-curators  sync-effects-atom async-effects-atom]
  (let [graph-value         (get-graph-value graph-manager-value)]
    (reduce
      (fn [{:keys [node-state] :as graph-manager-value} descriptor]
        (let [old-state (get node-state descriptor)
              {:keys [sync-effects async-effects]
               :as new-state} (finalize-tx
                                old-state
                                graph-value
                                resolver
                                descriptor)]
          (assert-valid-finalized-node-state new-state (:name descriptor))
          (when (not-empty sync-effects)
            (vswap! sync-effects-atom into! sync-effects))
          (when (not-empty async-effects)
            (vswap! async-effects-atom into! async-effects))
          (assoc-in
            graph-manager-value
            [:node-state
             descriptor]
            (cond-> new-state
              (not-empty sync-effects)
              (assoc :sync-effects [])
              (not-empty async-effects)
              (assoc :async-effects [])))))
      graph-manager-value
      disturbed-curators)))

(defn apply-effects
  [graph-manager sync-effects async-effects]
  (let [scheduler (.-scheduler graph-manager)]
    (g/-run-sync scheduler graph-manager sync-effects)
    (g/-run-async scheduler graph-manager async-effects)))

(s/fdef apply-command
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :curator any?
          :command vector?)
  :ret ::graph-manager-value)
(def recursion-limit 1000)

(defn -apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver descriptor command disturbed-curators]
  (assert (descriptor/descriptor? descriptor)
    (str "you must address commant to a descriptor not "
      (pr-str descriptor)
      " command "
      (pr-str command)))
  (let [dtor-impl   (resolver descriptor)
        dtor-kind   (:hitch2.descriptor.impl/kind dtor-impl)]
    (case dtor-kind
      :hitch2.descriptor.kind/curator
      (let [graph-value        (get-graph-value graph-manager-value)
            node-state         (get-init-node graph-manager-value resolver descriptor disturbed-curators)]
        (assoc-in graph-manager-value [:node-state descriptor]
          (if-some [apply-command (::curator-proto/apply-command dtor-impl)]
            (apply-command descriptor graph-value
              (tx-init-curator node-state graph-manager-value  resolver descriptor disturbed-curators)
              command)
            (assert false))))
      :hitch2.descriptor.kind/var
      (-apply-command graph-manager-value resolver (get-curator dtor-impl descriptor)
                     command disturbed-curators))))

(defn apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver descriptor command sync-effects-atom async-effects-atom]
  (let [disturbed-curators (volatile! (transient #{}))
        graph-manager-value (-apply-command graph-manager-value resolver descriptor command disturbed-curators)
        disturbed-curators-snapshot (persistent! @disturbed-curators)
        _  (vreset! disturbed-curators (transient disturbed-curators-snapshot))
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              disturbed-curators-snapshot
                              disturbed-curators
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! @disturbed-curators)
      sync-effects-atom async-effects-atom)
    ))

(defn apply-commands
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver cmds sync-effects-atom async-effects-atom]
  (let [disturbed-curators (volatile! (transient #{}))
        graph-manager-value
                           (reduce
                             (fn [gmv [descriptor command]]
                               (-apply-command gmv resolver descriptor command disturbed-curators))
                             graph-manager-value
                             cmds)
        disturbed-curators-snapshot (persistent! @disturbed-curators)
        _ (vreset! disturbed-curators (transient disturbed-curators-snapshot))
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              disturbed-curators-snapshot disturbed-curators
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! @disturbed-curators)
      sync-effects-atom async-effects-atom)
    ))

#_(defn to-curator [descriptor]
  (let [dtor-impl   (resolver descriptor)
        dtor-kind   (:hitch2.descriptor.impl/kind dtor-impl)]
    (case dtor-kind
      :hitch2.descriptor.kind/curator
      descriptor
      :hitch2.descriptor.kind/var
      (get-curator dtor-impl descriptor))))


(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager curator command]
    (let [sync-effects-atom (volatile! (transient []))
          async-effects-atom (volatile! (transient []))]
      (swap! state apply-command resolver curator command sync-effects-atom async-effects-atom)
      (apply-effects graph-manager
        (persistent! @sync-effects-atom)
        (persistent! @async-effects-atom))
      (:graph-value @state)))
  (-transact-commands! [graph-manager cmds]
    (let [sync-effects-atom (volatile! (transient []))
          async-effects-atom (volatile! (transient []))]
      (swap! state apply-commands resolver cmds sync-effects-atom async-effects-atom)
      (apply-effects graph-manager
        (persistent! @sync-effects-atom)
        (persistent! @async-effects-atom))
      (:graph-value @state)))
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  g/Inspect
  (-observed-by [gm descriptor]
    (get-in @state [:observed-by descriptor] NOT-IN-GRAPH-SENTINEL))
  (-observes [gm descriptor]
    (get-in @state [:observes descriptor] NOT-IN-GRAPH-SENTINEL))
  g/Resolver
  (-get-resolver [gm] resolver)
  )

(def default-scheduler
  #?(:clj (reify g/IScheduler
            (-run-sync [_ gm effects]
              (run! (fn [effect] (g/run-effect gm effect)) effects))
            (-run-async [_ gm effects]
              (run! (fn [effect] (g/run-effect gm effect)) effects)))
     :cljs (reify g/IScheduler
             (-run-sync [_ gm effects]
               (run! (fn [effect] (g/run-effect gm effect)) effects))
             (-run-async [_ gm effects]
               (goog.async.run (fn []
                                 (run! (fn [effect] (g/run-effect gm effect)) effects)))))))

(defn make-gm
  ([resolver] (make-gm resolver default-scheduler))
  ([resolver scheduler]
   (->gm (atom (->GraphManagerValue
                {}
                {}
                {}
                {}))
         scheduler
     resolver)))


