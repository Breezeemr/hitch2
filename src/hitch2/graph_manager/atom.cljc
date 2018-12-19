(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL NOT-IN-GRAPH-SENTINEL]]
             [hitch2.def.curator :as machine-proto]
             [hitch2.descriptor :as descriptor]
             [hitch2.protocols.tx-manager :as tx-manager-proto]
             [hitch2.tx-manager.halting :as halting-tx]
             [hitch2.halt :as halt])
  #?(:cljs (:import goog.async.run)))


(defrecord GraphManagerValue [graph-value
                              node-state
                              observes
                              observed-by])
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

(defn get-machine [impl sel]
  (if-some [f (:hitch2.descriptor.impl/get-machine impl)]
    (f sel)
    (assert false)))

(s/def ::selector any?)
(s/def ::graph-value (s/map-of ::selector any?))
(s/def ::derivation-state any?)
(s/def ::node-state (s/map-of
                      ::selector
                      (s/or
                        :curator-state
                        ::machine-proto/curator-state
                        :derivation-state
                        ::derivation-state)))

(s/def ::observes (s/map-of ::selector (s/coll-of ::selector)))
(s/def ::observed-by (s/map-of ::selector (s/coll-of ::selector)))

(s/def ::graph-manager-value
  (s/keys
    :req-un [::graph-value
             ::node-state
             ::observes
             ::observed-by]))

(defn get-observes [graph-manager-value selector]
  (-> graph-manager-value :observes (get selector)))

(defn get-observed-by [graph-manager-value selector]
  (-> graph-manager-value :observed-by (get selector)))

(defn get-node-state [graph-manager-value selector]
  (-> graph-manager-value :node-state (get selector)))

(defn get-graph-value [graph-manager-value]
  (-> graph-manager-value :graph-value))


(defn- add-to-working-set [working-set selector]
  (vswap! working-set conj! selector)
  nil)

(defn tx-init-machine [node-state graph-manager-value resolver selector disturbed-machines]
  (if (contains? @disturbed-machines selector)
    node-state
    (do
      (add-to-working-set disturbed-machines selector)
      (if-some [tx-init (::machine-proto/tx-init (resolver selector))]
        (tx-init selector (get-graph-value graph-manager-value) node-state)
        node-state))))

(declare propagate-dependency-changes)

(defn propagate-set-projections [graph-manager-value set-projections worklist-atom]
  (reduce-kv (fn [gv sel value]
               (let [old-value (-> gv :graph-value (get sel NOT-FOUND-SENTINEL))]
                 (if (= old-value value)
                   gv
                   (if (identical? value NOT-FOUND-SENTINEL)
                     (-> gv
                         (assoc-in [:node-state sel] (->var-state true))
                         (update :graph-value dissoc sel))
                     (do
                       (add-to-working-set worklist-atom sel)
                       (-> gv
                           (assoc-in [:node-state sel :value-changed?] true)
                           (assoc-in [:graph-value sel] value))
                       )))))
    graph-manager-value
    set-projections))

(defn halting [selector simpl tx-manager]
  (halt/maybe-halt
    ((:hitch2.descriptor.impl/halting simpl) tx-manager
      selector)
    NOT-FOUND-SENTINEL))
;todo partial evaluate the destructuring and return an clojure that takes a graph.

(defn run-halting [graph-manager-value
                   node-state
                   resolver
                   selector
                   simpl
                   worklist-atom
                   dirty-machines]
  (let [old-value (-> graph-manager-value :graph-value (get selector NOT-FOUND-SENTINEL))
        old-deps  (-> graph-manager-value :observes (get selector #{}))
        tx-manager (halting-tx/halting-manager (:graph-value graph-manager-value))
        ;;; NOTE: change this line to switch halting implementations
        new-value (halting selector simpl tx-manager)
        deps (tx-manager-proto/finish-tx! tx-manager)
        value-changed? (and (not= new-value old-value) (not (identical? new-value NOT-FOUND-SENTINEL)))
        added-deps       (into #{} (remove old-deps) deps)
        change-focus (-> {}
                           (into (map (fn [dep]
                                        [dep true]))
                             added-deps)
                           (into (comp (remove deps)
                                   (map (fn [dep]
                                          [dep false])))
                                 old-deps))]
    (when value-changed?
      (add-to-working-set worklist-atom selector))
    (cond-> (assoc-in
              graph-manager-value
              [:node-state selector]
              (cond-> node-state
                value-changed?
                (assoc
                  :value-changed?
                  true)
                (not-empty added-deps)
                (assoc
                  :waiting
                  (into #{} (remove (:graph-value graph-manager-value)) deps))))
      value-changed?
      (assoc-in
        [:graph-value selector]
        new-value)
      (not-empty change-focus)
      (propagate-dependency-changes resolver selector change-focus worklist-atom dirty-machines))))


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

(defn propagate-value-changes [graph-manager-value resolver parent worklist-atom dirty-machines]
  (reduce
    (fn [graph-manager-value selector]
      (let [sel-impl (resolver selector)
            sel-kind (:hitch2.descriptor.impl/kind sel-impl)
            node-state (get-node-state graph-manager-value selector)]
        (assert node-state)
        (case sel-kind
          :hitch2.descriptor.kind/machine
          (let [graph-value    (get-graph-value graph-manager-value)
                {:keys [sync-effects async-effects]
                 new-set-projections     :set-projections
                 new-change-focus :change-focus
                 :as                new-node-state}
                (if-some [observed-value-changes (::machine-proto/observed-value-changes sel-impl)]
                  (observed-value-changes selector graph-value
                    (tx-init-machine node-state graph-manager-value  resolver  selector dirty-machines)
                    #{parent})
                  (assert false))]
            (s/assert ::machine-proto/curator-state new-node-state)
            (when (or (not-empty new-set-projections)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (add-to-working-set worklist-atom selector))
            (cond-> (assoc-in graph-manager-value
                      [:node-state selector]
                      new-node-state)
              (not-empty new-change-focus)
              (->
                (assoc-in
                  [:node-state selector :change-focus] {})
                (propagate-dependency-changes resolver selector new-change-focus worklist-atom dirty-machines))))
          ;:hitch2.descriptor.kind/var
          ;(assert false "should not happen")
          :hitch2.descriptor.kind/halting
          (let [{:keys [waiting] :as node-state}
                (-> graph-manager-value
                    :node-state
                    (get selector NOT-FOUND-SENTINEL)
                    (update :waiting disj parent))]
            (assert (not= node-state NOT-FOUND-SENTINEL))
            (if (empty? waiting)
              (let [graph-manager-value (run-halting
                                          graph-manager-value
                                          node-state
                                          resolver
                                          selector
                                          sel-impl
                                          worklist-atom
                                          dirty-machines)]
                graph-manager-value)
              (assoc-in graph-manager-value [:node-state selector] node-state))))))
    graph-manager-value
    (-> graph-manager-value :observed-by (get parent))))


(defn propagate-node-changes [resolver worklist-atom dirty-machines]
  (fn [graph-manager-value selector]
    (let [sel-impl (resolver selector)
          sel-kind (:hitch2.descriptor.impl/kind sel-impl)
          node-state (get-node-state graph-manager-value selector)]
      (assert node-state)
      (case sel-kind
        :hitch2.descriptor.kind/machine
        (let [{:keys [change-focus set-projections]}
              node-state]
          (s/assert ::machine-proto/curator-state node-state)
          (when (not-empty set-projections)
            (add-to-working-set worklist-atom selector))
          (when *trace* (record! [:node-changes :machine (:name sel-impl)
                                  selector
                                  node-state]))
          (cond-> graph-manager-value
            (not-empty change-focus)
            (->
              (update-in [:node-state selector]
                assoc
                :change-focus {})
              (propagate-dependency-changes resolver selector change-focus worklist-atom dirty-machines))
            (not-empty set-projections)
            (->
              (update-in [:node-state selector]
                assoc
                :set-projections {})
              (propagate-set-projections set-projections worklist-atom))))
        :hitch2.descriptor.kind/var
        (let [{:keys [value-changed?]}
              node-state]
          (when *trace* (record! [:node-changes :var (:name sel-impl)
                                  selector]))
          (cond-> graph-manager-value
            value-changed?
            (->
              (update-in [:node-state selector]
                assoc
                :value-changed? false)
              (propagate-value-changes resolver selector worklist-atom dirty-machines))))
        :hitch2.descriptor.kind/halting
        (let [{:keys [value-changed? change-focus]}
              node-state]
          (when *trace*
            (record! [:node-changes :halting (:name sel-impl)
                      selector (-> graph-manager-value :graph-value (get selector))])
                )
          (cond-> graph-manager-value
            (not-empty change-focus)
            (->
              (update-in [:node-state selector]
                assoc
                :change-focus {})
              (propagate-dependency-changes resolver selector change-focus worklist-atom dirty-machines))
            value-changed?
            (->
              (update-in [:node-state selector]
                assoc
                :value-changed? false)
              (propagate-value-changes resolver selector worklist-atom dirty-machines))))))))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::selectors))
  :ret ::graph-manager-value)

(defn flush-tx [node-state graph-manager-value resolver  selector]
  (if-some [flush-tx (::machine-proto/flush-tx  (resolver selector))]
    (flush-tx selector (:graph-value graph-manager-value) node-state)
    node-state))

(defn flush-worklist [graph-manager-value resolver dirty-machines-snapshot flush-worklist-atom]
  (reduce
    (fn [graph-manager-value machine]
      (let [old-node-state (get-node-state graph-manager-value machine)
            new-node-sate  (flush-tx old-node-state graph-manager-value resolver machine)]
        (assert old-node-state)
        (if (= old-node-state new-node-sate)
          graph-manager-value
          (do (add-to-working-set flush-worklist-atom machine)
            (assoc-in graph-manager-value [:node-state machine] new-node-sate)))))
    graph-manager-value
    dirty-machines-snapshot))

(defn propagate-changes [graph-manager-value resolver work-list dirty-machines recursion-limit]
  (let [new-work-list-atom (volatile! (transient #{}))
        graph-manager-value (reduce
                                  (propagate-node-changes
                                    resolver
                                    new-work-list-atom
                                    dirty-machines)
                                  graph-manager-value
                                  work-list)]
    (assert (not (zero? recursion-limit)))
    (if-some [new-work-list (not-empty (persistent! @new-work-list-atom))]
      (recur graph-manager-value resolver new-work-list dirty-machines (dec recursion-limit))
      (let [dirty-machines-snapshot (persistent! @dirty-machines)
            _ (vreset! dirty-machines (transient dirty-machines-snapshot))
            flush-worklist-atom (volatile! (transient #{}))
            graph-manager-value (flush-worklist graph-manager-value resolver dirty-machines-snapshot flush-worklist-atom)]
        (if-some [flush-worklist (not-empty (persistent! @flush-worklist-atom))]
          (recur graph-manager-value resolver flush-worklist dirty-machines (dec recursion-limit))
          graph-manager-value)))))

(defn get-init-node [graph-manager-value resolver selector disturbed-machines]
  (if-some [node-state (get-node-state graph-manager-value selector)]
    (let [sel-impl (resolver selector)
          sel-kind (:hitch2.descriptor.impl/kind sel-impl)]
      node-state)
    (let [sel-impl (resolver selector)
          sel-kind (:hitch2.descriptor.impl/kind sel-impl)]
      (case sel-kind
        :hitch2.descriptor.kind/machine
        (if-some [init (::machine-proto/init (resolver selector))]
          (init selector)
          machine-proto/initial-curator-state)
        :hitch2.descriptor.kind/var
        nil
        :hitch2.descriptor.kind/halting
        nil
        )))

  )
(defn apply-child-change-commands [graph-manager-value resolver child changes worklist-atom dirty-machines]
  (reduce-kv
    (fn [graph-manager-value parent added|removed]
      (let [sel-impl   (resolver parent)
            sel-kind   (:hitch2.descriptor.impl/kind sel-impl)
            node-state (get-init-node graph-manager-value resolver parent dirty-machines)]
        (case sel-kind
          :hitch2.descriptor.kind/machine
          (let [graph-value        (get-graph-value graph-manager-value)
                {:keys [sync-effects async-effects]
                 new-change-focus :change-focus
                 set-projections         :set-projections
                 :as                new-node-state}
                (if-some [curation-changes (::machine-proto/curation-changes sel-impl)]
                  (curation-changes parent graph-value
                    (tx-init-machine node-state graph-manager-value  resolver parent dirty-machines)
                    (when added|removed
                      #{child})
                    (when (not added|removed)
                      #{child}))
                  (assert false))]
            (s/assert ::machine-proto/curator-state new-node-state)
            (when (or (not-empty set-projections)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (add-to-working-set worklist-atom parent))
            (when *trace*
              (record! [:child-change :machine
                        (:name sel-impl)]))
            (let [new-graph-manager-value
                  (cond->
                    (assoc-in graph-manager-value
                      [:node-state parent]
                      (assoc new-node-state
                        :change-focus {}))
                    (not-empty new-change-focus)
                    (propagate-dependency-changes resolver parent new-change-focus worklist-atom dirty-machines))]
              (case added|removed
                true new-graph-manager-value
                false (-> ;deinit lifecycle
                        (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by parent]))]
                          new-graph-manager-value
                          new-graph-manager-value)))))
          :hitch2.descriptor.kind/var
          (let [machine (get-machine sel-impl parent)]
            (assert (descriptor/descriptor? machine) (pr-str parent))
            (when *trace*
              (record! [:child-change :var
                        (:name sel-impl)]))
            (case added|removed
              true (if (get-in graph-manager-value [:observes parent machine])
                     graph-manager-value
                     (propagate-dependency-changes graph-manager-value resolver parent {machine true} worklist-atom dirty-machines))
              false (->                                     ;deinit
                      (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by parent]))]
                        graph-manager-value
                        (-> graph-manager-value
                            (update :graph-value dissoc parent)
                            (propagate-dependency-changes resolver parent {machine false} worklist-atom dirty-machines)))
                      (update :node-state dissoc parent))))
          :hitch2.descriptor.kind/halting
          (let [node-state (get-in graph-manager-value [:node-state parent] NOT-FOUND-SENTINEL)]
            (when *trace*
              (record! [:child-change :halting
                        (:name sel-impl)
                        child
                        parent]))
            (case added|removed
              true (if (= node-state NOT-FOUND-SENTINEL)
                     (let [graph-manager-value (run-halting
                                                 graph-manager-value
                                                 (->deriving-state
                                                   [] #{} false)
                                                 resolver
                                                 parent
                                                 sel-impl
                                                 worklist-atom
                                                 dirty-machines)]

                       (add-to-working-set worklist-atom parent)
                       graph-manager-value)
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
                                dirty-machines))
                          graph-manager-value)
                        (update :node-state dissoc parent))))))))
    graph-manager-value
    changes))

(defn update-observed-by [observed-by selector changes]
  (reduce-kv
    (fn [acc focus add|remove]
      (if add|remove
        (addval acc focus selector)
        (remove-val acc focus selector)))
    observed-by
    changes))

(defn update-observes [observes selector changes]
  (reduce-kv
    (fn [acc focus add|remove]
      (if add|remove
        (addval acc selector focus)
        (remove-val acc selector focus)))
    observes
    changes)
  )

(defn propagate-dependency-changes [graph-manager-value resolver selector changes worklist-atom dirty-machines]
  (apply-child-change-commands
    (-> graph-manager-value
        (update :observed-by update-observed-by selector changes)
        (update :observes update-observes selector changes))
    resolver
    selector
    changes
    worklist-atom
    dirty-machines))

(s/fdef -apply-command
  :args (s/cat
          :curator-state  ::machine-proto/curator-state
          :machine any?
          :command vector?
          :graph-value ::graph-value
          :observed-by (s/coll-of ::selector)
          :observes (s/coll-of ::selector))
  :ret ::machine-proto/curator-state)

(defn remove-effects [node-state machines]
  (reduce
    (fn [acc machine]
      (update acc machine assoc :sync-effects [] :async-effects []))
    node-state
    machines))

(defn finalize-tx [node-state graph-value resolver selector]
  (if-some [finalize (::machine-proto/finalize (resolver selector))]
    (finalize selector graph-value node-state)
    node-state))

(defn assert-valid-finalized-node-state [{:keys [change-focus set-projections]} selector-name]
  (assert (empty? change-focus) selector-name)
  (assert (empty? set-projections) selector-name))

(defn into! [target source]
  (reduce
    conj!
    target
    source))

(defn finalize-effects
  [graph-manager-value resolver disturbed-machines  sync-effects-atom async-effects-atom]
  (let [graph-value         (get-graph-value graph-manager-value)]
    (reduce
      (fn [{:keys [node-state] :as graph-manager-value} selector]
        (let [old-state (get node-state selector)
              {:keys [sync-effects async-effects]
               :as new-state} (finalize-tx
                                old-state
                                graph-value
                                resolver
                                selector)]
          (assert-valid-finalized-node-state new-state (:name selector))
          (when (not-empty sync-effects)
            (vswap! sync-effects-atom into! sync-effects))
          (when (not-empty async-effects)
            (vswap! async-effects-atom into! async-effects))
          (assoc-in
            graph-manager-value
            [:node-state
             selector]
            (cond-> new-state
              (not-empty sync-effects)
              (assoc :sync-effects [])
              (not-empty async-effects)
              (assoc :async-effects [])))))
      graph-manager-value
      disturbed-machines)))

(defn apply-effects
  [graph-manager sync-effects async-effects]
  (let [scheduler (.-scheduler graph-manager)]
    (g/-run-sync scheduler graph-manager sync-effects)
    (g/-run-async scheduler graph-manager async-effects)))

(s/fdef apply-command
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :machine any?
          :command vector?)
  :ret ::graph-manager-value)
(def recursion-limit 1000)

(defn -apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver selector command disturbed-machines]
  (assert (descriptor/descriptor? selector)
    (str "you must address commant to a selector not "
      (pr-str selector)
      " command "
      (pr-str command)))
  (let [sel-impl   (resolver selector)
        sel-kind   (:hitch2.descriptor.impl/kind sel-impl)]
    (case sel-kind
      :hitch2.descriptor.kind/machine
      (let [graph-value        (get-graph-value graph-manager-value)
            node-state         (get-init-node graph-manager-value resolver selector disturbed-machines)]
        (assoc-in graph-manager-value [:node-state selector]
          (if-some [apply-command (::machine-proto/apply-command sel-impl)]
            (apply-command selector graph-value
              (tx-init-machine node-state graph-manager-value  resolver selector disturbed-machines)
              command)
            (assert false))))
      :hitch2.descriptor.kind/var
      (-apply-command graph-manager-value resolver (get-machine sel-impl selector)
                     command disturbed-machines))))

(defn apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver selector command sync-effects-atom async-effects-atom]
  (let [disturbed-machines (volatile! (transient #{}))
        graph-manager-value (-apply-command graph-manager-value resolver selector command disturbed-machines)
        disturbed-machines-snapshot (persistent! @disturbed-machines)
        _  (vreset! disturbed-machines (transient disturbed-machines-snapshot))
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              disturbed-machines-snapshot
                              disturbed-machines
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! @disturbed-machines)
      sync-effects-atom async-effects-atom)
    ))

(defn apply-commands
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver cmds sync-effects-atom async-effects-atom]
  (let [disturbed-machines (volatile! (transient #{}))
        graph-manager-value
                           (reduce
                             (fn [gmv [selector command]]
                               (-apply-command gmv resolver selector command disturbed-machines))
                             graph-manager-value
                             cmds)
        disturbed-machines-snapshot (persistent! @disturbed-machines)
        _ (vreset! disturbed-machines (transient disturbed-machines-snapshot))
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              disturbed-machines-snapshot disturbed-machines
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! @disturbed-machines)
      sync-effects-atom async-effects-atom)
    ))

#_(defn to-machine [selector]
  (let [sel-impl   (resolver selector)
        sel-kind   (:hitch2.descriptor.impl/kind sel-impl)]
    (case sel-kind
      :hitch2.descriptor.kind/machine
      selector
      :hitch2.descriptor.kind/var
      (get-machine sel-impl selector))))


(deftype gm [state scheduler resolver]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (let [sync-effects-atom (volatile! (transient []))
          async-effects-atom (volatile! (transient []))]
      (swap! state apply-command resolver machine command sync-effects-atom async-effects-atom)
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
  (-observed-by [gm selector]
    (get-in @state [:observed-by selector] NOT-IN-GRAPH-SENTINEL))
  (-observes [gm selector]
    (get-in @state [:observes selector] NOT-IN-GRAPH-SENTINEL))
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


