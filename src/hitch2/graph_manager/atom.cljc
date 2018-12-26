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
                              observed-by])

(defprotocol PropagateValueChange
  (-propagate-value-change [node-state graph-manager-value descriptor parent resolver worklist-atom
                            dirty-curators]))

(defprotocol PropagateNodeChanges
  (-propagate-node-change [node-state graph-manager-value descriptor resolver worklist-atom
                           dirty-curators]))

(defprotocol ApplyChildChangeCommand
  (-apply-child-change-command [node-state graph-manager-value observer observed added|removed resolver worklist-atom
                                dirty-curators]))


(defrecord curator-state [node dtor-impl observes])
(defrecord deriving-state [change-focus waiting value-changed? dtor-impl observes])
(defrecord var-state [value-changed? dtor-impl observes])

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

(defn get-node-state [graph-manager-value descriptor]
  (-> graph-manager-value :node-state (get descriptor)))

(defn get-graph-value [graph-manager-value]
  (-> graph-manager-value :graph-value))


(defn- add-to-working-set [working-set descriptor]
  (vswap! working-set conj! descriptor)
  nil)

(defn tx-init-curator [node-state graph-manager-value descriptor disturbed-curators]
  (if (contains? @disturbed-curators descriptor)
    node-state
    (do
      (add-to-working-set disturbed-curators descriptor)
      (if-some [tx-init (::curator-proto/tx-init (:dtor-impl node-state))]
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
        (->curator-state
          (if-some [init (::curator-proto/init dtor-impl)]
            (init descriptor)
            curator-proto/initial-curator-state)
          dtor-impl
          #{})
        :hitch2.descriptor.kind/var
        (let [curator (get-curator dtor-impl descriptor)
              v (->var-state false dtor-impl #{curator})]
          v)
        :hitch2.descriptor.kind/halting
        (->deriving-state
          {} #{} false dtor-impl #{})
        ))))


(defn update-graph-value [gv dtor value]
  (if (identical? value NOT-FOUND-SENTINEL)
    (dissoc gv dtor)
    (assoc gv dtor value)))

(defn propagate-set-projections [graph-manager-value set-projections worklist-atom]
  (reduce-kv (fn [gv dtor value]
               ;(prn  dtor value (-> gv :node-state keys #_(get dtor)))

               (if-some [node-state (-> gv :node-state (get dtor))]
                 (let [old-value (-> gv :graph-value (get dtor NOT-FOUND-SENTINEL))]
                   (if (= old-value value)                  ;ignore setting same value
                     gv
                     (do (assert (instance? var-state node-state))
                         (when-not (identical? value NOT-FOUND-SENTINEL)
                           (add-to-working-set worklist-atom dtor))
                         (-> gv
                             (assoc-in [:node-state dtor]
                               (assoc node-state
                                 :value-changed?
                                 #_true
                                 (if (identical? value NOT-FOUND-SENTINEL)
                                   false
                                   true)))
                             (update :graph-value update-graph-value dtor value)))))
                 gv))
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
        old-deps  (-> node-state :observes)
        tx-manager (halting-tx/halting-manager (:graph-value graph-manager-value))
        ;;; NOTE: change this line to switch halting implementations
        new-value (halting descriptor simpl tx-manager)
        deps (tx-manager-proto/finish-tx! tx-manager)
        value-changed? (and (not= new-value old-value) (not (identical? new-value NOT-FOUND-SENTINEL)))
        waiting-deps   (into #{}
                         (comp
                           (remove (:graph-value graph-manager-value))
                           (remove old-deps))
                         deps)
        change-focus (-> {}
                           (into
                             (comp
                               (remove old-deps)
                               (map (fn [dep]
                                      [dep true])))
                             deps)
                           (into (comp (remove deps)
                                   (map (fn [dep]
                                          [dep false])))
                                 old-deps))]
    (when (or value-changed? (not-empty change-focus))
      (add-to-working-set worklist-atom descriptor))
    (cond-> (assoc-in
              graph-manager-value
              [:node-state descriptor]
              (cond->
                (assoc node-state :observes deps)
                value-changed?
                (assoc
                  :value-changed?
                  true)
                (not-empty waiting-deps)
                (assoc
                  :waiting
                  waiting-deps)
                ;(not-empty change-focus)
                #_(assoc
                  :change-focus
                  change-focus)))
      :always
      (update
        :graph-value
        update-graph-value descriptor new-value)
      (not-empty change-focus)
      (propagate-dependency-changes resolver descriptor change-focus worklist-atom dirty-curators)
      )))


(defn addval [x k v]
  (if-some [a (get x k)]
    (assoc! x k (conj a v))
    (assoc! x k (conj #{} v))))

(defn remove-val [x k v]
  (if-some [a (get x k)]
    (if-some [nv (not-empty (disj a v))]
      (assoc! x k nv)
      (dissoc! x k))
    x))
(defn assert-nc [node-state]
  (assert (instance? curator-state node-state) (pr-str node-state))
  node-state)
(defn assert-not-nc [node-state]
  (assert (not (instance? curator-state node-state)) (pr-str node-state))
  node-state)



;curator-state
;deriving-state
;var-state

(extend-protocol PropagateValueChange
  curator-state
  (-propagate-value-change [node-state graph-manager-value descriptor parent resolver worklist-atom
                            dirty-curators]
    (let [_           (assert (instance? curator-state node-state) (pr-str node-state))
          n           (:node node-state)
          graph-value (get-graph-value graph-manager-value)
          {:keys               [sync-effects async-effects]
           new-set-projections :set-projections
           new-change-focus    :change-focus
           :as                 new-node-state}
          (if-some [observed-value-changes (::curator-proto/observed-value-changes (:dtor-impl node-state))]
            (observed-value-changes descriptor graph-value
              (tx-init-curator n graph-manager-value descriptor dirty-curators)
              #{parent})
            (assert false))]
      (s/assert ::curator-proto/curator-state new-node-state)
      (when (or (not-empty new-set-projections)
              (not-empty sync-effects)
              (not-empty async-effects))
        (add-to-working-set worklist-atom descriptor))
      (cond-> (assoc-in graph-manager-value
                [:node-state descriptor :node]
                new-node-state)
        (not-empty new-change-focus)
        (->
          (assoc-in
            [:node-state descriptor :node :change-focus] {})
          (propagate-dependency-changes resolver descriptor new-change-focus worklist-atom dirty-curators)))))
  deriving-state
  (-propagate-value-change [node-state graph-manager-value descriptor parent  resolver worklist-atom
                            dirty-curators]
    (assert node-state)
    (let [{:keys [waiting] :as node-state}
          (-> node-state
              (update :waiting disj parent))]
      (if (empty? waiting)
        (run-halting
          graph-manager-value
          node-state
          resolver
          descriptor
          (:dtor-impl node-state)
          worklist-atom
          dirty-curators)
        (assoc-in graph-manager-value [:node-state descriptor] node-state))))
  ;var-state
  ;(-propagate-value-change [node-state ])
  )
(defn propagate-value-changes [graph-manager-value resolver parent worklist-atom dirty-curators]
  (reduce
    (fn [graph-manager-value descriptor]
      (-propagate-value-change (get-node-state graph-manager-value descriptor)
        graph-manager-value descriptor parent
        resolver worklist-atom dirty-curators))
    graph-manager-value
    (-> graph-manager-value :observed-by (get parent))))



;curator-state
;deriving-state
;var-state
(defn apply-curator-change-focus [{{:keys [change-focus] :as node} :node
                                   observes :observes :as node-state}]
  (assoc node-state
    :node (assoc node :change-focus {})
    :observes
    (persistent!
      (reduce-kv
        (fn [acc dtor add|remove]
          (if add|remove
            (conj! acc dtor)
            (disj! acc dtor)))
        (transient observes)
        change-focus)))

  )

(extend-protocol PropagateNodeChanges
  curator-state
  (-propagate-node-change [node-state graph-manager-value descriptor resolver worklist-atom
                            dirty-curators]
    (let [_ (assert (instance? curator-state node-state) (pr-str node-state))
          {:keys [change-focus set-projections]
           :as n}
          (:node node-state)]
      (s/assert ::curator-proto/curator-state n)
      (when (not-empty set-projections)
        (add-to-working-set worklist-atom descriptor))
      (cond-> graph-manager-value
        (not-empty change-focus)
        (->
          (update-in [:node-state descriptor]
            apply-curator-change-focus)
          (propagate-dependency-changes resolver descriptor change-focus worklist-atom dirty-curators))
        (not-empty set-projections)
        (->
          (update-in [:node-state descriptor :node]
            assoc
            :set-projections {})
          (propagate-set-projections set-projections worklist-atom)))))
  deriving-state
  (-propagate-node-change [node-state graph-manager-value descriptor  resolver worklist-atom
                            dirty-curators]
    (let [{:keys [value-changed? change-focus]}
          node-state]
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
          (propagate-value-changes resolver descriptor worklist-atom dirty-curators))))
    )
  var-state
  (-propagate-node-change [node-state graph-manager-value descriptor  resolver worklist-atom
                           dirty-curators]
    (let [{:keys [value-changed?]}
          node-state]
      (cond-> graph-manager-value
        value-changed?
        (->
          (update-in [:node-state descriptor]
            assoc
            :value-changed? false)
          (propagate-value-changes resolver descriptor worklist-atom dirty-curators))))))

(defn propagate-node-changes [resolver worklist-atom dirty-curators]
  (fn [graph-manager-value descriptor]
    (-propagate-node-change (get-node-state graph-manager-value descriptor)
      graph-manager-value descriptor
      resolver worklist-atom dirty-curators)))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::descriptors))
  :ret ::graph-manager-value)

(defn flush-tx [{n :node dtor-impl :dtor-impl
                 :as node-state} graph-manager-value resolver  descriptor]
  (assert (instance? curator-state node-state) (pr-str node-state))
  (if-some [flush-tx (::curator-proto/flush-tx  dtor-impl)]
    (assoc node-state :node
                      (flush-tx descriptor (:graph-value graph-manager-value) n))
    node-state))

(defn flush-worklist [graph-manager-value resolver dirty-curators-snapshot flush-worklist-atom]
  (reduce
    (fn [graph-manager-value curator]
      (let [old-node-state (assert-nc (get-node-state graph-manager-value curator))
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



(extend-protocol ApplyChildChangeCommand
  curator-state
  (-apply-child-change-command [node-state graph-manager-value  observer observed added|removed resolver worklist-atom
                           dirty-curators]
    (let [_                  (assert (instance? curator-state node-state) (pr-str node-state))
          n (:node node-state)
          graph-value        (get-graph-value graph-manager-value)
          {:keys [sync-effects async-effects]
           new-change-focus :change-focus
           set-projections         :set-projections
           :as                new-node-state}
          (if-some [curation-changes (::curator-proto/curation-changes (:dtor-impl node-state))]
            (curation-changes observed graph-value
              (tx-init-curator n graph-manager-value observed dirty-curators)
              (when added|removed
                #{observer})
              (when (not added|removed)
                #{observer}))
            (assert false))]
      (s/assert ::curator-proto/curator-state new-node-state)
      (when (or (not-empty set-projections)
              (not-empty sync-effects)
              (not-empty async-effects))
        (add-to-working-set worklist-atom observed))
      (let [new-graph-manager-value
            (cond->
              (assoc-in graph-manager-value
                [:node-state observed]
                (-> (assoc node-state
                      :node new-node-state)
                    apply-curator-change-focus))
              (not-empty new-change-focus)
              (propagate-dependency-changes resolver observed new-change-focus worklist-atom dirty-curators))]
        (case added|removed
          true new-graph-manager-value
          false (-> ;deinit lifecycle
                  (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
                    new-graph-manager-value
                    new-graph-manager-value))))))
  deriving-state
  (-apply-child-change-command [node-state graph-manager-value observer observed added|removed  resolver worklist-atom
                           dirty-curators]
    (let [old-node-state (get-in graph-manager-value [:node-state observed] NOT-FOUND-SENTINEL)]
      (case added|removed
        true (if (identical? old-node-state NOT-FOUND-SENTINEL)
               (run-halting
                 graph-manager-value
                 node-state
                 resolver
                 observed
                 (:dtor-impl node-state)
                 worklist-atom
                 dirty-curators)
               graph-manager-value)
        false (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
                graph-manager-value
                (->                                   ;deinit
                  (if-some [observes (not-empty (get node-state :observes))]
                    (-> graph-manager-value
                        (update :graph-value dissoc observed)
                        (propagate-dependency-changes
                          resolver
                          observed
                          (into {}
                            (map (fn [x]
                                   [x false]))
                            observes)
                          worklist-atom
                          dirty-curators))
                    graph-manager-value)
                  (update :node-state dissoc observed)))))
    )
  var-state
  (-apply-child-change-command [node-state graph-manager-value observer observed added|removed  resolver worklist-atom
                           dirty-curators]
    (let [old-state (-> graph-manager-value :node-state (get observed))]
      (case added|removed
        true
        (if old-state
          graph-manager-value
          (-> (assoc-in graph-manager-value [:node-state observed] node-state)
            (propagate-dependency-changes resolver observed
              (into {}
                (map (fn [dtor] [dtor true]))
                (get node-state :observes))
              worklist-atom dirty-curators)))
        false (->                                     ;deinit
                (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
                  graph-manager-value
                  (-> graph-manager-value
                      (update :graph-value dissoc observed)
                      (propagate-dependency-changes resolver observed
                        (into {}
                          (map (fn [dtor] [dtor false]))
                          (get node-state :observes))
                        worklist-atom dirty-curators)
                      (update :node-state dissoc observed))))))
    ))

(defn apply-child-change-commands [graph-manager-value resolver child changes worklist-atom dirty-curators]
  (reduce-kv
    (fn [graph-manager-value parent added|removed]
      (-apply-child-change-command (get-init-node graph-manager-value resolver parent dirty-curators)
        graph-manager-value child parent added|removed resolver worklist-atom
        dirty-curators))
    graph-manager-value
    changes))

(defn update-observed-by [observed-by descriptor changes]
  (persistent!
    (reduce-kv
      (fn [acc focus add|remove]
        (if add|remove
          (addval acc focus descriptor)
          (remove-val acc focus descriptor)))
      (transient observed-by)
      changes)))

(defn propagate-dependency-changes [graph-manager-value resolver descriptor changes worklist-atom dirty-curators]
  (apply-child-change-commands
    (-> graph-manager-value
        (update :observed-by update-observed-by descriptor changes))
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

(defn finalize-tx [{node :node :as node-state} graph-value resolver descriptor]
  (if-some [finalize (::curator-proto/finalize (:dtor-impl node-state))]
    (assoc node-state :node (finalize descriptor graph-value node))
    node-state))

(defn assert-valid-finalized-node-state [{{:keys [change-focus set-projections] } :node} descriptor-name]
  (assert (empty? change-focus) descriptor-name)
  (assert (empty? set-projections) descriptor-name))

(defn into! [target source]
  (reduce
    conj!
    target
    source))


(defn remove-effects [{{:keys [sync-effects async-effects]
                        :as node} :node :as node-state}]
  (assoc node-state :node
                    (cond-> node
                      (not-empty sync-effects)
                      (assoc :sync-effects [])
                      (not-empty async-effects)
                      (assoc :async-effects []))))

(defn finalize-effects
  [graph-manager-value resolver disturbed-curators  sync-effects-atom async-effects-atom]
  (let [graph-value         (get-graph-value graph-manager-value)]
    (reduce
      (fn [{:keys [node-state] :as graph-manager-value} descriptor]
        (let [{{:keys [sync-effects async-effects]}
                     :node :as   new-state}
              (finalize-tx
                (get node-state descriptor)
                                graph-value
                                resolver
                                descriptor)]
          (assert-valid-finalized-node-state new-state (:name descriptor))
          (when (not-empty sync-effects)
            (vswap! sync-effects-atom into! sync-effects))
          (when (not-empty async-effects)
            (vswap! async-effects-atom into! async-effects))
          (update-in
            graph-manager-value
            [:node-state
             descriptor]
            remove-effects)))
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
            node-state   (get-init-node graph-manager-value resolver descriptor disturbed-curators)
            n (:node node-state)]
        (assert (instance? curator-state node-state) (pr-str node-state))
        (assoc-in graph-manager-value [:node-state descriptor]
          (assoc node-state :node
                            (if-some [apply-command (::curator-proto/apply-command dtor-impl)]
                              (apply-command descriptor graph-value
                                (tx-init-curator n graph-manager-value descriptor disturbed-curators)
                                command)
                              (assert false)))))
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
    (get-in @state [:node-state descriptor :observes] NOT-IN-GRAPH-SENTINEL))
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
                {}))
         scheduler
     resolver)))


