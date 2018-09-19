(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.protocols.machine :as machine-proto]
             [hitch2.protocols.selector :as selector-proto]))


(s/def ::selector any?)
(s/def ::value (s/map-of ::selector any?))
(s/def ::derivation-state any?)
(s/def ::node-state (s/map-of
                      ::selector
                      (s/or
                        :machine-state
                        :hitch2.protocols.machine/node-state
                        :derivation-state
                        ::derivation-state)))

(s/def ::parents (s/map-of ::selector (s/coll-of ::selector)))
(s/def ::children (s/map-of ::selector (s/coll-of ::selector)))

(s/def ::graph-manager-value
  (s/keys
    :req-un [::value
             ::node-state
             ::parents
             ::children]))

(defn ensure-machine-init [state machine]
  (if-some [m (get-in state [:node-state machine])]
    state
    (assoc-in state [:node-state machine]
      (machine-proto/-initialize machine))))

(defn n0 [x]
  (nth x 0))
(defn n1 [x]
  (nth x 1))
(defn n2 [x]
  (nth x 1))


(defn apply-parent-change-command [node-state machine-instance graph-manager-value children parents changes]
  (let [parent-selectors (into #{} (map n0) changes)]
    (machine-proto/-parent-value-changes machine-instance graph-manager-value node-state children
                                         parents parent-selectors)))

(defn populate-new-var-values [graph-manager-value var-resets]
  (reduce (fn [gv [_parent sel value]]
            (update-in graph-manager-value [:value sel] value))
          graph-manager-value
          var-resets))

(defn apply-var-resets [graph-manager-value changes]
  (reduce (fn [g [parent changes-for-parent]]
            (case (selector-proto/selector-kind parent)
              :hitch.selector.kind/machine
              (update-in g [:node-state parent] apply-parent-change-command
                parent
                (-> graph-manager-value :value)
                (get-in g [:children parent])
                (get-in g [:parents parent])
                changes-for-parent)))
          (populate-new-var-values graph-manager-value changes)
          (group-by n1 changes)))

(defn apply-child-change-command [node-state machine-instance graph-manager-value children parents changes]
  (let [{children-added   true
         children-removed false}
        (group-by n2 changes)]
    (machine-proto/-child-changes machine-instance graph-manager-value node-state children parents
                                  children-added children-removed)))

(defn apply-child-change-commands [graph-manager-value changes]
  (reduce (fn [acc [parent changes]]
            (case (selector-proto/selector-kind parent)
              :hitch.selector.kind/machine
              (update-in acc [:node-state parent] apply-child-change-command
                parent
                (-> graph-manager-value :value)
                (-> graph-manager-value :children (get parent))
                (-> graph-manager-value :parents (get parent))
                changes)))
          graph-manager-value
          (group-by n1 changes)))

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

(defn apply-parent-change [graph-manager-value [child parent add|remove]]
  (if add|remove
    (-> graph-manager-value
        (update :parents addval child parent)
        (update :children addval parent child))
    (-> graph-manager-value
        (update :parents removeval child parent)
        (update :children removeval parent child))))

(defn apply-parent-changes [graph-manager-value changes]
  (reduce apply-parent-change
          (apply-child-change-commands graph-manager-value changes)
          changes))

(defn pop-vars&parents [graph-manager-value machines]
  (reduce (fn [[graph-manager-value resets changes] machine]
            (let [{:keys [reset-vars parent-changes]} (get-in graph-manager-value
                                                              [:node-state machine])]
              [(update-in graph-manager-value [:node-state machine]
                          merge {:reset-vars {} :parent-changes {}})
               (into resets (map (fn [[sel value]]
                                   [machine sel value])
                                 reset-vars))
               (into changes (map (fn [[parent add|remove]
                                       [machine parent add|remove]])
                                  parent-changes))]))
          [graph-manager-value [] []]
          machines))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::selectors))
  :ret ::graph-manager-value)

(defn propagate-changes [graph-manager-value dirty-list]
  (loop [graph-manager-value        graph-manager-value
         dirty-list         dirty-list
         disturbed-machines (into #{} dirty-list)]
    (let [[graph-manager-value var-resets parent-changes] (pop-vars&parents graph-manager-value dirty-list)
          new-dirty                    (-> #{}
                                           (into (map n1) parent-changes)
                                           (into (map n1) var-resets))]
      (if (seq new-dirty)
        (recur
          (-> graph-manager-value
              (apply-parent-changes parent-changes)
              (apply-var-resets var-resets))
          new-dirty
          (-> disturbed-machines
              (into new-dirty)))
        [graph-manager-value disturbed-machines]))))

(s/fdef -apply-command
  :args (s/cat
          :machine-state  :hitch2.protocols.machine/node-state
          :machine any?
          :command vector?
          :value ::value
          :children (s/coll-of ::selector)
          :parents (s/coll-of ::selector))
  :ret :hitch2.protocols.machine/node-state)

(defn -apply-command [machine-state machine command graph-value children parents]
  ;; what is value here? it's called with :value from the gv but we've
  ;; been calling graph-manager-value the map value of the graph atom. Can
  ;; this function be called as an update to :node-state as it is?
  (machine-proto/-apply-command machine graph-value machine-state children parents command))

(defn apply-effects
  ""
  [graph-manager-value disturbed-machines])



(s/fdef apply-command
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :machine any?
          :command vector?)
  :ret ::graph-manager-value)

(defn apply-command
  ""
  [graph-manager-value machine command]
  (let [{:keys [parents children]
         graph-value :value
         :as initalized-graph}
        (ensure-machine-init graph-manager-value machine)
        ;;init-tx node-state contains _all_ machine states. do we need
        ;; an update-in [:node-state machine]?
        command-applied-graph (update-in initalized-graph [:node-state machine]
                                         -apply-command machine command graph-value children parents)
        [propagated-graph disturbed-machines] (propagate-changes command-applied-graph [machine])
        ;;flush-tx
        ]
    (apply-effects propagated-graph disturbed-machines)))

(deftype gm [state]
  g/Snapshot
  (-get-graph [graph-manager]
    (:value @state))
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (:value (swap! state apply-command machine command)))
  (-transact-commands! [graph-manager cmds])
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  )

(defn make-gm []
  (->gm (atom {:value {}
               :node-state {}
               :parents {}
               :children {}})))


