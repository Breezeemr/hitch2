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

(s/def ::graph-value
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


(defn apply-parent-change-command [node-state machine-instance graph-value children parents changes]
  (let [parent-selectors (into #{} (map n0) changes)]
    (machine-proto/-parent-value-changes machine-instance graph-value node-state children
                                         parents parent-selectors)))

(defn apply-parent-change-commands [graph-value changes]
  (reduce (fn [acc [parent changes]]
            (update-in acc [:node-state parent]
                       apply-parent-change-command
                       parent
                       (-> graph-value :value)
                       (-> graph-value :children (get parent))
                       (-> graph-value :parents (get parent))
                       changes))
          graph-value
          (group-by n1 changes)))

(defn apply-var-reset [graph-value [parent sel value]]
  (update-in graph-value [:value sel] value))


(defn apply-var-resets [graph-value changes]
  (reduce apply-var-reset
          (apply-parent-change-commands graph-value changes)
          changes))

(defn apply-child-change-command [node-state machine-instance graph-value children parents changes]
  (let [{children-added   true
         children-removed false}
        (group-by n2 changes)]
    (machine-proto/-child-changes machine-instance graph-value node-state children parents
                                  children-added children-removed)))

(defn apply-child-change-commands [graph-value changes]
  (reduce (fn [acc [parent changes]]
            (update-in acc [:node-state parent] apply-child-change-command
                       parent
                       (-> graph-value :value)
                       (-> graph-value :children (get parent))
                       (-> graph-value :parents (get parent))
                       changes))
          graph-value
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

(defn apply-parent-change [graph-value [child parent add|remove]]
  (if add|remove
    (-> graph-value
        (update :parents addval child parent)
        (update :children addval parent child))
    (-> graph-value
        (update :parents removeval child parent)
        (update :children removeval parent child))))

(defn apply-parent-changes [graph-value changes]
  (reduce apply-parent-change
          (apply-child-change-commands graph-value changes)
          changes))

(defn pop-vars&parents [graph-value machines]
  (reduce (fn [[graph-value resets changes] machine]
            (let [{:keys [reset-vars parent-changes]} (get-in graph-value
                                                              [:node-state machine])]
              [(update-in graph-value [:node-state machine]
                          merge {:reset-vars {} :parent-changes {}})
               (into resets (map (fn [[sel value]]
                                   [machine sel value])
                                 reset-vars))
               (into changes (map (fn [[parent add|remove]
                                       [child parent add|remove]])
                                  parent-changes))]))
          [graph-value [] []]
          machines))

(s/fdef propagate-changes
  :args (s/cat
          :graph-value  ::graph-value
          :dirty-list (s/coll-of ::selectors))
  :ret ::graph-value)

(defn propagate-changes [graph-value dirty-list]
  (loop [graph-value        graph-value
         dirty-list         dirty-list
         disturbed-machines (into #{} dirty-list)]
    (let [[graph-value var-resets parent-changes] (pop-vars&parents graph-value dirty-list)
          new-dirty                    (-> #{}
                                           (into (map n1) parent-changes)
                                           (into (map n1) var-resets))]
      (if (seq new-dirty)
        (recur
          (-> graph-value
              (apply-parent-changes parent-changes)
              (apply-var-resets var-resets))
          new-dirty
          (-> disturbed-machines
              (into new-dirty)))
        [graph-value disturbed-machines]))))

(s/fdef -apply-command
  :args (s/cat
          :machine-state  :hitch2.protocols.machine/node-state
          :machine any?
          :command vector?
          :value ::value
          :children (s/coll-of ::selector)
          :parents (s/coll-of ::selector))
  :ret :hitch2.protocols.machine/node-state)

(defn -apply-command [machine-state machine command value children parents]
  (machine-proto/-apply-command machine
    value machine-state children parents
    command))

(defn apply-effects
  ""
  [graph-value disturbed-machines])



(s/fdef apply-command
  :args (s/cat
          :graph-value  ::graph-value
          :machine any?
          :command vector?)
  :ret ::graph-value)

(defn apply-command
  ""
  [graph-value machine command]
  (let [{:keys [value parents children] :as initalized-graph}
        (ensure-machine-init graph-value machine)
        ;init-tx
        command-applied-graph (update initalized-graph :node-state -apply-command  machine command value children parents)
        [propagated-graph disturbed-machines] (propagate-changes command-applied-graph [machine])
        ;flush-tx
        ]
    (apply-effects propagated-graph disturbed-machines)))

(deftype gm [state]
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


