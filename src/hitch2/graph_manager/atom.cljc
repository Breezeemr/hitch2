(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
             [hitch2.protocols.machine :as machine-proto]
             [hitch2.protocols.selector :as selector-proto]
             [hitch2.protocols.tx-manager :as tx-manager-proto]
             [hitch2.tx-manager.halting :as halting-tx]
             [hitch2.halt :as halt]))



(defrecord deriving-state [change-parent waiting value-changed?])
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

(s/def ::selector any?)
(s/def ::graph-value (s/map-of ::selector any?))
(s/def ::derivation-state any?)
(s/def ::node-state (s/map-of
                      ::selector
                      (s/or
                        :machine-state
                        ::machine-proto/machine-state
                        :derivation-state
                        ::derivation-state)))

(s/def ::parents (s/map-of ::selector (s/coll-of ::selector)))
(s/def ::children (s/map-of ::selector (s/coll-of ::selector)))

(s/def ::graph-manager-value
  (s/keys
    :req-un [::graph-value
             ::node-state
             ::parents
             ::children]))


(defn init-machine [node-state machine]
  (let [machine-state (if node-state
                        node-state
                        (machine-proto/-initialize machine))]
    (s/assert ::machine-proto/machine-state machine-state)
    machine-state))

(defn tx-init-machine [node-state graph-manager-value selector disturbed-machines]
  (if (contains? @disturbed-machines selector)
    node-state
    (do
      (swap! disturbed-machines conj selector)
      (machine-proto/-init-tx
          selector
          (-> graph-manager-value :graph-value)
          node-state
          (-> graph-manager-value :children (get selector))
          (-> graph-manager-value :parents (get selector))))))

(defn ensure-inits [node-state graph-manager-value selector disturbed-machines]
  (-> node-state
      (init-machine selector)
      (tx-init-machine graph-manager-value selector disturbed-machines)))

(declare propagate-dependency-changes)

(defn propagate-reset-vars [graph-manager-value reset-vars worklist-atom]
  (reduce-kv (fn [gv sel value]
               (let [old-value (get-in gv [:graph-value sel] NOT-FOUND-SENTINEL)]
                 (if (= old-value value)
                   gv
                   (if (identical? value NOT-FOUND-SENTINEL)
                     (-> gv
                         (assoc-in [:node-state sel] (->var-state true))
                         (update :graph-value dissoc sel))
                     (do
                       (swap! worklist-atom conj sel)
                       (-> gv
                           (assoc-in [:node-state sel :value-changed?] true)
                           (assoc-in [:graph-value sel] value))
                       )))))
    graph-manager-value
    reset-vars))

(defn run-halting [graph-manager-value
                   node-state
                   selector
                   h-fn
                   worklist-atom
                   dirty-machines]
  (let [old-value (get-in graph-manager-value [:graph-value selector] NOT-FOUND-SENTINEL)
        old-deps  (get-in graph-manager-value [:parents selector])
        tx-manager (halting-tx/halting-manager (:graph-value graph-manager-value))
        new-value (halt/maybe-halt
             (selector-proto/-invoke-halting selector h-fn tx-manager)
             NOT-FOUND-SENTINEL)
        deps (tx-manager-proto/finish-tx! tx-manager)
        value-changed? (and (not= new-value old-value) (not (identical? new-value NOT-FOUND-SENTINEL)))
        added-deps       (into [] (remove old-deps) deps)
        change-parent (-> []
                           (into (map (fn [dep]
                                        [selector dep true]))
                             added-deps)
                           (into (comp (remove deps)
                                   (map (fn [dep]
                                          [selector dep false])))
                             old-deps))]
    (when value-changed?
      (swap! worklist-atom conj selector))
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
                  added-deps)))
      value-changed?
      (assoc-in
        [:graph-value selector]
        new-value)
      (not-empty change-parent)
      (propagate-dependency-changes selector change-parent worklist-atom dirty-machines))))


(defn addval [x k v]
  (if-some [a (get x k)]
    (assoc x k (conj a v))
    (assoc x k (conj #{} v))))

(defn removeval [x k v]
  (if-some [a (get x k)]
    (if-some [nv (not-empty (disj a v))]
      (assoc x k nv)
      (dissoc x k))
    x))

(defn remove-all [items to-remove]
  (persistent!
    (reduce
      disj!
      (transient items)
      to-remove)))

(defn propagate-value-changes [graph-manager-value parent worklist-atom dirty-machines]
  (reduce
    (fn [graph-manager-value selector]
      (let [sel-impl (selector-proto/-imp selector)
            sel-kind (selector-proto/-imp-kind sel-impl)
            node-state (get-in graph-manager-value [:node-state selector])]
        (case sel-kind
          :hitch.selector.kind/machine
          (let [graph-value    (-> graph-manager-value :graph-value)
                children       (-> graph-manager-value :children (get selector))
                parents        (-> graph-manager-value :parents (get selector))
                {:keys [sync-effects async-effects]
                 new-reset-vars     :reset-vars
                 new-change-parent :change-parent
                 :as                new-node-state}
                (machine-proto/-parent-value-changes
                  selector
                  graph-value
                  (ensure-inits node-state graph-manager-value selector dirty-machines)
                  children
                  parents
                  #{parent})]
            (s/assert ::machine-proto/machine-state new-node-state)
            (when (or (not-empty new-reset-vars)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (swap! worklist-atom conj selector))
            (cond-> (assoc-in graph-manager-value
                      [:node-state selector]
                      new-node-state)
              (not-empty new-change-parent)
              (->
                (assoc-in
                  [:node-state selector :change-parent] {})
                (propagate-dependency-changes selector new-change-parent worklist-atom dirty-machines))))
          ;:hitch.selector.kind/var
          ;(assert false "should not happen")
          :hitch.selector.kind/halting
          (let [{:keys [waiting] :as node-state}
                (-> (get-in graph-manager-value [:node-state selector] NOT-FOUND-SENTINEL)
                    (update :waiting disj selector))]
            (assert (not= node-state NOT-FOUND-SENTINEL))
            (if (empty? waiting)
              (let [graph-manager-value (run-halting
                                          graph-manager-value
                                          node-state
                                          selector
                                          (selector-proto/-get-halting-fn sel-impl)
                                          worklist-atom
                                          dirty-machines)]
                graph-manager-value)
              graph-manager-value)))))
    graph-manager-value
    (get-in graph-manager-value [:children parent])))

(defn propagate-node-changes [worklist-atom dirty-machines]
  (fn [graph-manager-value selector]
    (let [sel-impl (selector-proto/-imp selector)
          sel-kind (selector-proto/-imp-kind sel-impl)
          node-state (get-in graph-manager-value [:node-state selector])]
      (case sel-kind
        :hitch.selector.kind/machine
        (let [{:keys [change-parent reset-vars]}
              node-state]
          (s/assert ::machine-proto/machine-state node-state)
          (when (not-empty reset-vars)
            (swap! worklist-atom conj selector))
          (when *trace* (record! [:node-changes :machine (selector-proto/-sname sel-impl)
                                  node-state]))
          (cond-> graph-manager-value
            (not-empty change-parent)
            (->
              (update-in [:node-state selector]
                assoc
                :change-parent {})
              (propagate-dependency-changes selector change-parent worklist-atom dirty-machines))
            (not-empty reset-vars)
            (->
              (update-in [:node-state selector]
                assoc
                :reset-vars {})
              (propagate-reset-vars reset-vars worklist-atom))))
        :hitch.selector.kind/var
        (let [{:keys [value-changed?]}
              node-state]
          (when *trace* (record! [:node-changes :var (selector-proto/-sname sel-impl)]))
          (cond-> graph-manager-value
            value-changed?
            (->
              (update-in [:node-state selector]
                assoc
                :value-changed? false)
              (propagate-value-changes selector worklist-atom dirty-machines))))
        :hitch.selector.kind/halting
        (let [{:keys [value-changed? change-parent]}
              node-state]
          (cond-> graph-manager-value
            (not-empty change-parent)
            (->
              (update-in [:node-state selector]
                assoc
                :change-parent {})
              (propagate-dependency-changes selector change-parent worklist-atom dirty-machines))
            value-changed?
            (->
              (update-in [:node-state selector]
                assoc
                :value-changed? false)
              (propagate-value-changes selector worklist-atom dirty-machines))))))))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::selectors))
  :ret ::graph-manager-value)

(defn flush-tx [node-state graph-manager-value selector]
  (machine-proto/-flush-tx
    selector
    (:graph-value graph-manager-value)
    node-state
    (-> graph-manager-value :children (get selector))
    (-> graph-manager-value :parents (get selector))))

(defn flush-worklist [graph-manager-value dirty-machines-snapshot flush-worklist-atom]
  (reduce
    (fn [graph-manager-value machine]
      (let [old-node-state (get-in graph-manager-value [:node-state machine])
            new-node-sate  (flush-tx old-node-state graph-manager-value machine)]
        (if (= old-node-state new-node-sate)
          graph-manager-value
          (do (swap! flush-worklist-atom conj machine)
            (assoc-in graph-manager-value [:node-state machine] new-node-sate)))))
    graph-manager-value
    dirty-machines-snapshot))

(defn propagate-changes [graph-manager-value work-list dirty-machines recursion-limit]
  (let [new-work-list-atom (atom [])
        graph-manager-value (reduce
                                  (propagate-node-changes
                                    new-work-list-atom
                                    dirty-machines)
                                  graph-manager-value
                                  work-list)]
    (assert (not (zero? recursion-limit)))
    (if-some [new-work-list (not-empty @new-work-list-atom)]
      (recur graph-manager-value new-work-list dirty-machines (dec recursion-limit))
      (let [dirty-machines-snapshot @dirty-machines
            flush-worklist-atom (atom #{})
            graph-manager-value (flush-worklist graph-manager-value dirty-machines-snapshot flush-worklist-atom)]
        (if-some [flush-worklist (not-empty @flush-worklist-atom)]
          (recur graph-manager-value flush-worklist dirty-machines (dec recursion-limit))
          graph-manager-value)))))

(defn apply-child-change-commands [graph-manager-value child changes worklist-atom dirty-machines]
  (reduce-kv
    (fn [graph-manager-value parent added|removed]
      (let [sel-impl   (selector-proto/-imp parent)
            sel-kind   (selector-proto/-imp-kind sel-impl)
            node-state (get-in graph-manager-value [:node-state parent])]
        (case sel-kind
          :hitch.selector.kind/machine
          (let [graph-value        (-> graph-manager-value :graph-value)
                children           (-> graph-manager-value :children (get parent))
                parents            (-> graph-manager-value :parents (get parent))
                {:keys [sync-effects async-effects]
                 new-change-parent :change-parent
                 reset-vars         :reset-vars
                 :as                new-node-state}
                (machine-proto/-child-changes
                  parent
                  graph-value
                  (ensure-inits node-state graph-manager-value parent dirty-machines)
                  children
                  parents
                  (when added|removed
                    #{child})
                  (when (not added|removed)
                    #{child}))]
            (s/assert ::machine-proto/machine-state new-node-state)
            (when (or (not-empty reset-vars)
                    (not-empty sync-effects)
                    (not-empty async-effects))
              (swap! worklist-atom conj parent))
            (cond->
              (assoc-in graph-manager-value
                [:node-state parent]
                (assoc new-node-state
                  :change-parent {}))
              (not-empty new-change-parent)
              (propagate-dependency-changes  parent new-change-parent worklist-atom dirty-machines)))
          :hitch.selector.kind/var
          (let [machine (selector-proto/-get-machine sel-impl parent)]
            (case added|removed
              true (if (get-in graph-manager-value [:parents parent machine])
                     graph-manager-value
                     (propagate-dependency-changes graph-manager-value parent {machine true} worklist-atom dirty-machines))
              false (if-some [children (not-empty (get-in graph-manager-value [:children parent]))]
                      graph-manager-value
                      (-> graph-manager-value
                          (update :graph-value dissoc parent)
                          (propagate-dependency-changes parent {machine false} worklist-atom dirty-machines)))))
          :hitch.selector.kind/halting
          (let [node-state (get-in graph-manager-value [:node-state parent] NOT-FOUND-SENTINEL)]
            (case added|removed
              true (if (= node-state NOT-FOUND-SENTINEL)
                     (let [graph-manager-value (run-halting
                                                 graph-manager-value
                                                 (->deriving-state
                                                   [] #{} false)
                                                 parent
                                                 (selector-proto/-get-halting-fn sel-impl)
                                                 worklist-atom
                                                 dirty-machines)]

                       (swap! worklist-atom conj parent)
                       graph-manager-value)
                     (not added|removed))
              false (if-some [parents (not-empty (get-in graph-manager-value [:parents parent]))]
                      graph-manager-value
                      (if-some [children (not-empty (get-in graph-manager-value [:children parent]))]
                        (-> graph-manager-value
                            (update :graph-value dissoc parent)
                            (propagate-dependency-changes
                              parent
                              (into {}
                                (map (fn [x]
                                       [x false]))
                                children)
                              worklist-atom
                              dirty-machines))
                        graph-manager-value)))))))
    graph-manager-value
    changes))

(defn update-children [children child changes]
  (reduce-kv
    (fn [acc parent add|remove]
      (if add|remove
        (addval acc parent child)
        (removeval acc parent child)))
    children
    changes))

(defn update-parents [parents child changes]
  (reduce-kv
    (fn [acc parent add|remove]
      (if add|remove
        (addval acc child parent)
        (removeval acc child parent)))
    parents
    changes)
  )

(defn propagate-dependency-changes [graph-manager-value child changes worklist-atom dirty-machines]
  (let []
    (apply-child-change-commands
      (-> graph-manager-value
          (update :children update-children child changes)
          (update :parents update-parents child changes))
      child
      changes
      worklist-atom
      dirty-machines)))

(s/fdef -apply-command
  :args (s/cat
          :machine-state  ::machine-proto/machine-state
          :machine any?
          :command vector?
          :graph-value ::graph-value
          :children (s/coll-of ::selector)
          :parents (s/coll-of ::selector))
  :ret ::machine-proto/machine-state)

(defn remove-effects [node-state machines]
  (reduce
    (fn [acc machine]
      (update acc machine assoc :sync-effects [] :async-effects []))
    node-state
    machines))

(defn apply-effects
  ""
  [graph-manager-value graph-manager disturbed-machines]
  (let [node-state          (:node-state graph-manager-value)
        sync-effects        (into []
                              (mapcat
                                (fn [machine]
                                  (get-in node-state [machine :sync-effects])))
                              disturbed-machines)
        async-effects       (into []
                              (mapcat
                                (fn [machine]
                                  (get-in node-state [machine :async-effects])))
                              disturbed-machines)
        graph-manager-value (update graph-manager-value :node-state remove-effects disturbed-machines)]
    (run! (fn [effect] (g/run-effect graph-manager effect)) sync-effects)
    (run! (fn [effect] (g/run-effect graph-manager effect)) async-effects)
    graph-manager-value))

(s/fdef apply-command
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :machine any?
          :command vector?)
  :ret ::graph-manager-value)

(defn apply-command
  "Apply command to machine and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value selector command disturbed-machines]
  (let [sel-impl   (selector-proto/-imp selector)
        sel-kind   (selector-proto/-imp-kind sel-impl)
        node-state (get-in graph-manager-value [:node-state selector])]
    (case sel-kind
      :hitch.selector.kind/machine
      (let [graph-value        (-> graph-manager-value :graph-value)
            children           (-> graph-manager-value :children (get selector))
            parents            (-> graph-manager-value :parents (get selector))]
        (assoc-in graph-manager-value [:node-state selector]
          (machine-proto/-apply-command
            selector
            graph-value
            (ensure-inits node-state graph-manager-value selector disturbed-machines)
            children parents command)))
      :hitch.selector.kind/var
      (apply-command graph-manager-value (selector-proto/-get-machine sel-impl selector)
                     command disturbed-machines))))

(defn apply-commands
  "Apply command to machine and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value cmds disturbed-machines]
  (reduce
    (fn [gmv [selector command]]
      (apply-command gmv selector command disturbed-machines))
    graph-manager-value
    cmds))

(defn to-machine [selector]
  (let [sel-impl   (selector-proto/-imp selector)
        sel-kind   (selector-proto/-imp-kind sel-impl)]
    (case sel-kind
      :hitch.selector.kind/machine
      selector
      :hitch.selector.kind/var
      (selector-proto/-get-machine sel-impl selector))))
(def recursion-limit 1000000)

(deftype gm [state]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (let [disturbed-machines (atom #{})
          ;; pass disturbed machines into apply commands
          _ (swap! state apply-command machine command disturbed-machines)
          ;; or look at disturbed machines and if they are vars, map
          ;; them to their machines.
          _ (swap! state propagate-changes @disturbed-machines disturbed-machines recursion-limit)
          new-graph-manager-value (swap! state apply-effects graph-manager @disturbed-machines)]
      (:graph-value new-graph-manager-value)))
  (-transact-commands! [graph-manager cmds]
    (let [disturbed-machines (atom #{})
          _ (swap! state apply-commands cmds disturbed-machines)
          _ (swap! state propagate-changes @disturbed-machines disturbed-machines recursion-limit)
          new-graph-manager-value (swap! state apply-effects graph-manager @disturbed-machines)]
      (:graph-value new-graph-manager-value)))
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  )

(defn make-gm []
  (->gm (atom {:graph-value {}
               :node-state {}
               :parents {}
               :children {}})))


