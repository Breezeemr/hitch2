(ns hitch2.graph-manager.atom
  (:require  [clojure.spec.alpha :as s]
             [hitch2.protocols.graph-manager :as g]
             [hitch2.protocols.machine :as machine-proto]
             [hitch2.protocols.selector :as selector-proto]))





(s/def ::selector any?)
(s/def ::graph-value (s/map-of ::selector any?))
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
    :req-un [::graph-value
             ::node-state
             ::parents
             ::children]))


(defrecord halting-derive-state [waiting])

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
            (assoc-in graph-manager-value [:graph-value sel] value))
          graph-manager-value
          var-resets))

(defn apply-var-resets [graph-manager-value changes]
  (let [populated-gmv (populate-new-var-values graph-manager-value changes)]
   [populated-gmv
    (reduce (fn [acc [parent sel value]]
              (assert (= (selector-proto/selector-kind parent) :hitch.selector.kind/var-singleton-machine))
              (if (= (get-in populated-gmv [:graph-value sel]) value)
                acc
                ;;this assumes dependencies are updated first!!
                (reduce (fn [acc child]
                          (update acc child
                                  (fnil conj {}) sel))
                        acc
                        (get-in graph-manager-value [:children sel]))))
            {}
            changes)]))

(defn apply-child-change-command [node-state machine-instance graph-manager-value
                                  children parents changes]
  (let [{children-added   true
         children-removed false}
        (group-by n2 changes)]
    (machine-proto/-child-changes machine-instance graph-manager-value node-state children parents
                                  children-added children-removed)))

(defn apply-child-change-commands [graph-manager-value changes]
  (reduce (fn [[graph-manager-value var-resets parent-changes value-changes disturbed-machines1]
               [parent changes]]
            (let [sel-impl (selector-proto/-imp parent)
                  sel-kind (selector-proto/-imp-kind sel-impl)
                  node-state (get-in graph-manager-value [:node-state parent])]
              (case sel-kind
                :hitch.selector.kind/machine
                (let [{new-var-resets     :var-resets
                       new-parent-changes :parent-changes
                       :as                new-node-state}
                      (apply-child-change-command node-state parent
                                                  (-> graph-manager-value :graph-value)
                                                  (-> graph-manager-value :children (get parent))
                                                  (-> graph-manager-value :parents (get parent))
                                                  changes)]
                  [(assoc-in graph-manager-value [:node-state parent] (assoc new-node-state
                                                                             :var-resets {}
                                                                             :parent-changes {}))
                   (into var-resets new-var-resets)
                   (into parent-changes new-parent-changes)
                   ;; always untouched
                   value-changes disturbed-machines1])
                :hitch.selector.kind/var-singleton-machine
                (let []
                  [graph-manager-value
                   var-resets
                   ;;tell machines of dependency only if new
                   parent-changes
                   value-changes ;; always untouched
                   disturbed-machines1])
                :hitch.selector.kind/halting
                [graph-manager-value
                 var-resets
                 parent-changes
                 value-changes  ;; always untouched
                 disturbed-machines1])))
          ;; the last two value-changes and disturbed-machines1 (???)
          ;; is never modified and remain empty.
          [graph-manager-value {} {} {} #{}]
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
  (apply-child-change-commands (reduce apply-parent-change
                                       graph-manager-value
                                       changes)
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
               (into changes (map (fn [[parent add|remove]]
                                    [machine parent add|remove])
                                  parent-changes))]))
          [graph-manager-value [] []]
          machines))

(defn apply-value-change [graph-manager-value [child parent add|remove]]
  (if add|remove
    (-> graph-manager-value
        (update :parents addval child parent)
        (update :children addval parent child))
    (-> graph-manager-value
        (update :parents removeval child parent)
        (update :children removeval parent child))))

(defn apply-value-changes [graph-manager-value changes]
  (apply-child-change-commands (reduce apply-value-change
                                       graph-manager-value
                                       changes)
                               changes))

(s/fdef propagate-changes
  :args (s/cat
          :graph-manager-value  ::graph-manager-value
          :dirty-list (s/coll-of ::selectors))
  :ret ::graph-manager-value)

(defn propagate-changes [graph-manager-value dirty-list]
  (let [[graph-manager-value var-resets parent-changes]
        (pop-vars&parents graph-manager-value dirty-list)]
    (loop [graph-manager-value graph-manager-value
           var-resets          var-resets
           parent-changes      parent-changes
           ;; value changes is always empty so the recur is never taken
           value-changes       {}
           disturbed-machines  dirty-list]
      ;; should this be or?
      (if (and (not-empty var-resets) (not-empty parent-changes) (not-empty value-changes))
        (let [[graph-manager-value var-resets1 parent-changes1 value-changes1 disturbed-machines1]
              (apply-parent-changes graph-manager-value parent-changes)

              [graph-manager-value value-changes2] (apply-var-resets graph-manager-value var-resets)

              [graph-manager-value var-resets3 parent-changes3 value-changes3 disturbed-machines3]
              (apply-value-changes graph-manager-value parent-changes)]
          (recur
            graph-manager-value
            (into var-resets1 var-resets3)
            (into parent-changes1 parent-changes3)
            (into value-changes1 value-changes2) ;; what about value-changes 3?
            (-> disturbed-machines
                (into disturbed-machines1)
                (into disturbed-machines3))))
        [graph-manager-value disturbed-machines]))))

(s/fdef -apply-command
  :args (s/cat
          :machine-state  :hitch2.protocols.machine/node-state
          :machine any?
          :command vector?
          :graph-value ::graph-value
          :children (s/coll-of ::selector)
          :parents (s/coll-of ::selector))
  :ret :hitch2.protocols.machine/node-state)

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
  "Apply command to machine and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value machine command]
  (let [{:keys [graph-value parents children] :as initalized-graph}
        (ensure-machine-init graph-manager-value machine)

        command-applied-graph
        (update-in initalized-graph [:node-state machine]
                   (fn [machine-state]
                     (machine-proto/-apply-command machine graph-value machine-state
                                                   children parents command)))
        [propagated-graph disturbed-machines]
        (propagate-changes command-applied-graph [machine])
        ;;flush-tx
        ]
    (apply-effects propagated-graph disturbed-machines)))

(deftype gm [state]
  g/Snapshot
  (-get-graph [graph-manager]
    (:graph-value @state))
  g/GraphManagerSync
  (-transact! [graph-manager machine command]
    (:graph-value (swap! state apply-command machine command)))
  (-transact-commands! [graph-manager cmds])
  g/GraphManagerAsync
  (-transact-async! [graph-manager v command])
  (-transact-commands-async! [graph-manager cmds])
  )

(defn make-gm []
  (->gm (atom {:graph-value {}
               :node-state {}
               :parents {}
               :children {}})))


