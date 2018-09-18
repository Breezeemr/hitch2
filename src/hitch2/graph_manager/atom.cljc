(ns hitch2.graph-manager.atom
  (:require [hitch2.protocols.graph-manager :as g]
            [hitch2.protocols.machine :as machine-proto]))


(defn ensure-machine-init [state machine]
  (if-some [m (get-in state [:node-state machine])]
    state
    (assoc-in state [:node-state machine]
      (machine-proto/-initialize machine))))

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

(defn n0 [x]
  (nth x 0))
(defn n1 [x]
  (nth x 1))
(defn n2 [x]
  (nth x 1))


(defn apply-parent-change-command [node-state machine-instance graph-value children parents changes]
  (let [parent-selectors (into #{} (map n0) changes)]
    (machine-proto/-parent-value-changes machine-instance graph-value node-state children parents parent-selectors)
    ))

(defn apply-parent-change-commands [graph-value changes]
  (reduce
    (fn [acc [parent changes]]
      (update-in acc [:node-state parent] apply-parent-change-command
        parent
        (-> graph-value :value)
        (-> graph-value :children #(get % parent))
        (-> graph-value :parents #(get % parent))
        changes))
    graph-value
    (group-by n1 changes))

(defn apply-var-reset [graph-value [parent sel value]]
  (update-in graph-value [:value sel] value))


(defn apply-var-resets [graph-value changes]
  (reduce
    apply-var-reset
    graph-value
    changes))

(defn apply-child-change-command [node-state machine-instance graph-value children parents changes]
  (let [{children-added   true
         children-removed false}
        (group-by n2 changes)]
    (machine-proto/-child-changes machine-instance graph-value node-state children parents children-added children-removed)))

(defn apply-child-change-commands [graph-value changes]
  (reduce
    (fn [acc [parent changes]]
      (update-in acc [:node-state parent] apply-child-change-command
        parent
        (-> graph-value :value)
        (-> graph-value :children #(get % parent))
        (-> graph-value :parents #(get % parent))
        changes))
    graph-value
    (group-by n1 changes))
  )
(defn apply-parent-change [graph-value [child parent add|remove]]
  (if add|remove
    (-> graph-value
        (update  :parents addval child parent)
        (update  :children addval parent child))
    (-> graph-value
        (update  :parents removeval child parent)
        (update  :children removeval parent child))))

(defn apply-parent-changes [graph-value changes]
  (reduce
    apply-parent-change
    graph-value changes))

(defn remove-parent-changes [graph-value children]
  (reduce (fn [gv child]
            (assoc-in gv [:node-state child :parent-changes] {}))
    graph-value children))

(defn get-parent-changes [graph-value children]
  (let [nstates (:node-state graph-value)]
    (into []
      (mapcat (fn [child]
                (let [{changes :parent-changes} (get nstates child)]
                  (map
                    (fn [[parent add|remove]]
                      [child parent add|remove])
                    changes))))
      children)))

(defn remove-var-resets [graph-value machines]
  (reduce (fn [gv child]
            (assoc-in gv [:node-state child :reset-vars] {}))
    graph-value machines))

(defn get-var-resets [graph-value machines]
  (let [nstates (:node-state graph-value)]
    (into []
      (mapcat (fn [machine]
                (let [{changes :parent-changes} (get nstates child)]
                  (map
                    (fn [[sel value]]
                      [machine sel value])
                    changes))))
      machines)))


(defn propagate-changes [graph-value dirty-list]
  (loop [graph-value        graph-value
         change-parents     dirty-list
         reset-vars         dirty-list
         disturbed-machines (into #{} dirty-list)]
    (let [parent-changes (not-empty (get-parent-changes graph-value change-parents))
          new-graph      (remove-parent-changes graph-value change-parents)
          var-resets     (not-empty (get-var-resets new-graph reset-vars))
          new-graph      (remove-var-resets new-graph reset-vars)]
      (if (or parent-changes var-resets)
        (recur
          (-> new-graph
              (apply-child-change-commands parent-changes )
              (apply-parent-changes parent-changes)
              (apply-parent-change-commands parent-changes )
              (apply-var-resets var-resets))
          parent-changes
          var-resets
          (-> disturbed-machines
              (into (map n1) parent-changes)
              (into (map n0) var-resets)))
        new-graph)))
  )

(defn -apply-command [machine-state machine command value children parents]
  (machine-proto/-apply-command machine
    value machine-state children parents
    command))

(defn apply-effects [graph-value disturbed-machines])

(defn apply-command [graph-value machine command]
  (let [{:keys [value parents children] :as initalized-graph}
        (ensure-machine-init graph-value machine)
        ;init-tx
        command-applied-graph (update :node-state -apply-command  machine command value children parents)
        propagated-graph      (propagate-changes command-applied-graph [machine])
        ;flush-tx
        ]
    (apply-effects propagated-graph [])))

(deftype gm [state]
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (swap! state apply-command machine command tx-state))
  (-transact-commands! [graph-manager cmds])
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  )

(defn make-gm []
  (->gm (atom {:values {}
               :nodestate {}
               :parents {}
               :children {}})))


