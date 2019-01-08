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


(defn into! [target source]
  (reduce
    conj!
    target
    source))

(defn tinto! [target xform source]
  (transduce
    xform
    conj!
    target
    source))
(defn takeout! [target source]
  (reduce
    disj!
    target
    source))

(defn update! [tcoll k f arg1]
  (let [old-val (get tcoll k)]
    (assoc! tcoll k (f old-val arg1))))

(defn conj!-tset [coll nv]
  (if coll
    (conj! coll nv)
    (conj! (transient #{}) nv)))

(defrecord GraphManagerValue [graph-value
                              node-state
                              observed-by])

(defprotocol PropagateValueChange
  (-propagate-value-change [node-state graph-manager-value descriptor parent resolver worklist]))

(defprotocol ApplyChildChangeCommand
  (-apply-child-change-command [node-state graph-manager-value observed  changes resolver worklist]))


(defrecord curator-state [node dtor-impl observes])
(defrecord deriving-state [waiting dtor-impl observes])
(defrecord var-state [dtor-impl observes])

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

;(s/def ::descriptor any?)
;(s/def ::graph-value (s/map-of ::descriptor any?))
;(s/def ::derivation-state any?)
;(s/def ::node-state (s/map-of
;                      ::descriptor
;                      (s/or
;                        :curator-state
;                        ::curator-proto/curator-state
;                        :derivation-state
;                        ::derivation-state)))
;
;(s/def ::observes (s/map-of ::descriptor (s/coll-of ::descriptor)))
;(s/def ::observed-by (s/map-of ::descriptor (s/coll-of ::descriptor)))
;
;(s/def ::graph-manager-value
;  (s/keys
;    :req-un [::graph-value
;             ::node-state
;             ::observes
;             ::observed-by]))


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

(defn update!-observed-by [observed-by descriptor changes]
  (assert descriptor)
  (reduce-kv
    (fn [acc focus add|remove]
      (if add|remove
        (addval acc focus descriptor)
        (remove-val acc focus descriptor)))
    observed-by
    changes))

(defn update-observed-by [observed-by descriptor changes]
  (assert descriptor)
  (persistent!
    (update!-observed-by
      (transient (or observed-by #{}))
      descriptor
      changes)))

(defn update-observed-by-batch
  [{:keys [observed-by] :as gmv} batch]
  (assoc gmv
    :observed-by
    (persistent!
      (reduce
        (fn [acc [descriptor changes]]
          (update!-observed-by acc descriptor changes))
        (transient observed-by)
        batch))))

(defrecord focus-change [add-set rm-set])

(defn update!+- [tcoll k f arg1]
  (if-some [old-val (get tcoll k)]
    (do (f old-val arg1)
      tcoll)
    (assoc! tcoll k (f (->focus-change (transient #{}) (transient #{})) arg1 ))))

(defn add-dep [{:keys [add-set rm-set] :as fc} curator]
  (conj! add-set curator)
  (disj! rm-set curator)
  fc)

(defn rm-dep [{:keys [add-set rm-set] :as fc} curator]
  (disj! add-set curator)
  (conj! rm-set curator)
  fc)

(defn get-node-state [graph-manager-value descriptor]
  (-> graph-manager-value :node-state (get descriptor)))

(defn get-graph-value [graph-manager-value]
  (-> graph-manager-value :graph-value))


(defn tx-init-curator [node-state graph-manager-value descriptor worklist]
  (if (contains? (:in-tx-curators worklist) descriptor)
    node-state
    (do
      (conj! (:in-tx-curators worklist) descriptor)
      (if-some [tx-init (::curator-proto/tx-init (:dtor-impl node-state))]
        (tx-init descriptor (get-graph-value graph-manager-value) node-state)
        node-state))))


(defn get-init-node [graph-manager-value resolver descriptor worklist]
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
              v (->var-state dtor-impl #{curator})]
          v)
        :hitch2.descriptor.kind/halting
        (->deriving-state
          (transient #{}) dtor-impl #{})
        ))))


(defn update-graph-value [gv dtor value]
  (if (identical? value NOT-FOUND-SENTINEL)
    (dissoc gv dtor)
    (assoc gv dtor value)))

(defn propagate-set-projections [graph-manager-value set-projections worklist]
  (reduce-kv (fn [gv dtor value]
               ;(prn  dtor value (-> gv :node-state keys #_(get dtor)))

               (if-some [node-state (-> gv :node-state (get dtor))]
                 (let [old-value (-> gv :graph-value (get dtor NOT-FOUND-SENTINEL))]
                   (if (= old-value value)                  ;ignore setting same value
                     gv
                     (do (assert (instance? var-state node-state))
                         (when-not (identical? value NOT-FOUND-SENTINEL)
                           (let [{:keys [observed-by]} gv
                                 {:keys [value-changes]} worklist]
                             (run!
                               (fn [observer]
                                 (update! value-changes observer conj!-tset dtor))
                               (observed-by dtor))))
                         (update gv :graph-value update-graph-value dtor value))))
                 (do                                        ;observed-by(prn :node-gone dtor (-> gv :node-state keys))
                   gv)))
    graph-manager-value
    set-projections))

(defn halting [descriptor simpl tx-manager]
  (halt/maybe-halt
    ((:hitch2.descriptor.impl/halting simpl) tx-manager
      descriptor)
    NOT-FOUND-SENTINEL))
;todo partial evaluate the destructuring and return an clojure that takes a graph.

(defn run-halting [{:keys [observed-by] :as graph-manager-value}
                   node-state
                   resolver
                   descriptor
                   simpl
                   worklist]
  (let [old-value (-> graph-manager-value :graph-value (get descriptor NOT-FOUND-SENTINEL))
        old-deps  (-> node-state :observes)
        tx-manager (halting-tx/halting-manager (:graph-value graph-manager-value))
        ;;; NOTE: change this line to switch halting implementations
        new-value (halting descriptor simpl tx-manager)
        deps (tx-manager-proto/finish-tx! tx-manager)
        value-changed? (not= new-value old-value)
        waiting-deps   (tinto! (transient #{})
                         (comp
                           (remove (:graph-value graph-manager-value))
                           (remove old-deps))
                         deps)
        change-focus (if (= deps old-deps)
                       {}
                       (-> {}
                           (into
                             (comp
                               (remove old-deps)
                               (map (fn [dep]
                                      [dep true])))
                             deps)
                           (into (comp (remove deps)
                                   (map (fn [dep]
                                          [dep false])))
                             old-deps)))]
    (when (and value-changed? (not (identical? new-value NOT-FOUND-SENTINEL)))
      (let [{:keys [value-changes]} worklist]
        (run!
          (fn [observer]
            (update! value-changes observer conj!-tset descriptor))
          (observed-by descriptor))))
    (when (not-empty change-focus)
      (let [{change-focus-worklist :change-focus} worklist]
        (reduce-kv
          (fn [acc observed added?]
            (if added?
              (update!+- change-focus-worklist observed add-dep descriptor)
              (update!+- change-focus-worklist observed rm-dep descriptor)))
          nil
          change-focus)))
    (cond-> (assoc-in
              graph-manager-value
              [:node-state descriptor]
              (cond->
                (assoc node-state
                  :observes deps)
                (pos? (count waiting-deps))
                (assoc
                  :waiting
                  waiting-deps)))
      value-changed?
      (update
        :graph-value
        update-graph-value descriptor new-value)
      (not-empty change-focus)
      (update :observed-by update-observed-by descriptor change-focus))))

(defn do-recalcs [graph-manager-value resolver to-recalc worklist]
  (reduce
    (fn [graph-manager-value observed]
      (let [node-state (get-in graph-manager-value [:node-state observed] NOT-FOUND-SENTINEL)]
        (assert (not (identical? node-state NOT-FOUND-SENTINEL)))
        (run-halting
          graph-manager-value
          node-state
          resolver
          observed
          (:dtor-impl node-state)
          worklist)))
    graph-manager-value
    to-recalc))

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
  (-propagate-value-change [node-state graph-manager-value descriptor parents resolver worklist]
    (let [_           (assert (instance? curator-state node-state) (pr-str node-state))
          n           (:node node-state)
          graph-value (get-graph-value graph-manager-value)
          {:keys               [sync-effects async-effects]
           new-set-projections :set-projections
           new-change-focus    :change-focus
           :as                 new-node-state}
          (if-some [observed-value-changes (::curator-proto/observed-value-changes (:dtor-impl node-state))]
            (observed-value-changes descriptor graph-value
              (tx-init-curator n graph-manager-value descriptor worklist)
              parents)
            (assert false))]
      (when (not-empty new-set-projections)
        (let [{:keys [project-vars]} worklist]
          (into! project-vars new-set-projections)))
      (when (not-empty new-change-focus)
        (let [{:keys [change-focus]} worklist]
          (reduce-kv
            (fn [acc parent added?]
              (if added?
                (update!+- change-focus parent add-dep descriptor)
                (update!+- change-focus parent rm-dep descriptor)))
            nil
            new-change-focus)))
      (cond-> (assoc-in graph-manager-value
                [:node-state descriptor :node]
                (cond-> new-node-state
                  (not-empty new-change-focus)
                  (assoc :change-focus {})
                  (not-empty new-set-projections)
                  (assoc :set-projections {})))
        (not-empty new-change-focus)
        (update :observed-by update-observed-by descriptor new-change-focus))))
  deriving-state
  (-propagate-value-change [{:keys [waiting] :as node-state}
                            graph-manager-value descriptor parents  resolver worklist]
    (assert node-state)
    (takeout! waiting parents)
    (when (zero? (count waiting))
      (let [{:keys [recalc]} worklist]
        (conj! recalc descriptor)))
    graph-manager-value)
  ;var-state
  ;(-propagate-value-change [node-state ])
  )
(defn propagate-value-changes [graph-manager-value resolver changes worklist]
  (reduce-kv
    (fn [graph-manager-value descriptor parents]
      (if-some [node-state (get-node-state graph-manager-value descriptor)]
        (-propagate-value-change node-state
          graph-manager-value descriptor
          (persistent! parents)
          resolver worklist)
        graph-manager-value))
    graph-manager-value
    changes))



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

(defn flush-tx [{n :node dtor-impl :dtor-impl
                 :as node-state} graph-manager-value resolver  descriptor]
  (assert (instance? curator-state node-state) (pr-str node-state))
  (if-some [flush-tx (::curator-proto/flush-tx  dtor-impl)]
    (assoc node-state :node
                      (flush-tx descriptor (:graph-value graph-manager-value) n))
    node-state))

(defn curator-node->worklist! [work-list backrefs]
  (fn [node-state curator-dtor]
    (assert (instance? curator-state node-state) (pr-str node-state))
    (let [{:keys [change-focus set-projections]
           :as   n}
          (:node node-state)]
      ;(s/assert ::curator-proto/curator-state n)
      (when (not-empty change-focus)
        (let [{change-focus-worklist :change-focus} work-list]
          (reduce-kv
            (fn [acc observed added?]
              (if added?
                (update!+- change-focus-worklist observed add-dep curator-dtor)
                (update!+- change-focus-worklist observed rm-dep curator-dtor)))
            nil
            change-focus))
        (assert curator-dtor)
        (conj! backrefs [curator-dtor change-focus]))
      (when (not-empty set-projections)
        (let [{:keys [project-vars]} work-list]
          (into! project-vars set-projections)))
      (cond-> node-state
        (not-empty change-focus)
        apply-curator-change-focus
        (not-empty set-projections)
        (update :node
          assoc
          :set-projections {})))))

(defn blah [work-list backrefs]
  (let [flusher (curator-node->worklist! work-list backrefs)]
    (fn [graph-manager-value curator-dtor]
      (let [node-state (get-node-state graph-manager-value curator-dtor)
            _          (assert (instance? curator-state node-state) (pr-str node-state))
            flushed-state (flusher node-state curator-dtor)]
        (-> graph-manager-value
            (assoc-in
              [:node-state curator-dtor]
              flushed-state))))))


(defn flush-worklist [graph-manager-value resolver dirty-curators-snapshot worklist]
  (let [backrefs (transient {})
        flusher (curator-node->worklist! worklist backrefs)]
    (update-observed-by-batch
      (reduce
        (fn [graph-manager-value curator]
          (let [old-node-state (assert-nc (get-node-state graph-manager-value curator))
                new-node-state (flush-tx old-node-state graph-manager-value resolver curator)
                flushed-state  (flusher new-node-state curator)]
            ;(assert new-node-state)
            ;(assert old-node-state)
            (assoc-in graph-manager-value [:node-state curator] flushed-state)))
        graph-manager-value
        dirty-curators-snapshot)
      (persistent! backrefs))))

(defrecord wlist [touched-curators in-tx-curators project-vars change-focus value-changes recalc])

(defn make-work-list []
  (->wlist
    (transient (hash-set))
    (transient (hash-set))
    (transient (hash-map))
    (transient (hash-map))
    (transient (hash-map))
    (transient (hash-set))))

(defn init-worklist [graph-manager-value work-list]
  (let [disturbed-curators-snapshot (persistent! (:in-tx-curators @work-list))
        backrefs (transient {})]
    (vswap! work-list
      assoc
      :in-tx-curators
      (transient disturbed-curators-snapshot))
    (update-observed-by-batch
      (reduce
        (blah @work-list backrefs)
        graph-manager-value
        disturbed-curators-snapshot)
      (persistent! backrefs))))

(defn get-reset-worklist-part [worklist k]
  (let [val (get @worklist k)]
    (when (pos? (count val))
      (let [persistent-val (persistent! val)]
        (vswap! worklist assoc
          k (case k
              :in-tx-curators  (transient (hash-set))
              :project-vars (transient (hash-map))
              :change-focus (transient (hash-map))
              :value-changes (transient (hash-map))
              :recalc (transient (hash-set))
              ))
        persistent-val))))
(declare apply-focus-changes )
(defn trace [label x]
  (prn label x)
  x)
(defn propagate-changes
  "Main loop that propagates changes until the graph is settled"
  [graph-manager-value resolver work-list2 recursion-limit]
  (assert (not (zero? recursion-limit)))
  (if-some [set-projections (get-reset-worklist-part work-list2 :project-vars)]
    (do                                                     ;(prn (count set-projections) :set-projections)
        (recur (propagate-set-projections graph-manager-value set-projections @work-list2)
          resolver work-list2 (dec recursion-limit)))
    (if-some [value-changes (get-reset-worklist-part work-list2 :value-changes)]
      (do                                                   ;(prn (count value-changes) :value-changes)
          (recur (propagate-value-changes graph-manager-value resolver value-changes @work-list2)
            resolver work-list2 (dec recursion-limit)))
      (if-some [change-focus (get-reset-worklist-part work-list2 :change-focus)]
        (do                                                 ;(prn (count change-focus) :change-focus)
            (recur (apply-focus-changes graph-manager-value resolver change-focus @work-list2)
              resolver work-list2 (dec recursion-limit)))
        (if-some [recalcs (get-reset-worklist-part work-list2 :recalc)]
          (do                                               ;(prn (count recalcs) :recalcs)
              (recur (do-recalcs graph-manager-value resolver recalcs @work-list2)
                resolver work-list2 (dec recursion-limit)))
          (if-some [in-tx-curators (get-reset-worklist-part work-list2 :in-tx-curators)]
            (do                                             ;(prn (count in-tx-curators) :txends)
                 (into! (:touched-curators @work-list2) in-tx-curators)
              (recur (flush-worklist graph-manager-value resolver in-tx-curators @work-list2)
                resolver work-list2 (dec recursion-limit)))
            graph-manager-value))))))



(extend-protocol ApplyChildChangeCommand
  curator-state
  (-apply-child-change-command [node-state graph-manager-value  observed changes resolver worklist]
    (let [_                  (assert (instance? curator-state node-state) (pr-str node-state))
          n (:node node-state)
          graph-value        (get-graph-value graph-manager-value)
          {:keys [sync-effects async-effects]
           new-change-focus :change-focus
           set-projections         :set-projections
           :as                new-node-state}
          (if-some [curation-changes (::curator-proto/curation-changes (:dtor-impl node-state))]
            (curation-changes observed graph-value
              (tx-init-curator n graph-manager-value observed worklist)
              (-> changes :add-set persistent!)
              (-> changes :rm-set persistent!))
            (assert false))]
      ;(s/assert ::curator-proto/curator-state new-node-state)
      (when (not-empty set-projections)
        (let [{:keys [project-vars]} worklist]
          (into! project-vars set-projections)))
      (when (not-empty new-change-focus)
        (let [{:keys [change-focus]} worklist]
          (reduce-kv
            (fn [acc parent added?]
              (if added?
                (update!+- change-focus parent add-dep observed)
                (update!+- change-focus parent rm-dep observed)))
            nil
            new-change-focus)))
      (let [new-graph-manager-value
            (cond-> (assoc-in graph-manager-value
                  [:node-state observed]
                  (-> (assoc node-state
                        :node (if (not-empty set-projections)
                                (assoc new-node-state :set-projections {})
                                new-node-state))
                      apply-curator-change-focus))
              (not-empty new-change-focus)
              (update :observed-by  update-observed-by observed new-change-focus))]
        (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
          new-graph-manager-value
          new-graph-manager-value)  ;deinit lifecycle
          )))
  deriving-state
  (-apply-child-change-command [node-state graph-manager-value observed changes  resolver worklist]
    (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
      (if-some [old-state (-> graph-manager-value :node-state (get observed))]
        graph-manager-value
        (let [{:keys [recalc]} worklist]
          (conj! recalc observed)
          (assoc-in graph-manager-value
            [:node-state observed]
            node-state)))
      (->                                   ;deinit
        (if-some [observes (not-empty (get node-state :observes))]
          (let [{:keys [change-focus]} worklist]
            (run!
              (fn [parent]
                (update!+- change-focus parent rm-dep observed))
              observes)
            (-> graph-manager-value

                ;todo a bit of optimization could happen here
                (update :observed-by  update-observed-by observed (into {} (map (fn [x] [x false])) observes))
                ))
          graph-manager-value)
        (update :graph-value dissoc observed)
        (update :node-state dissoc observed)))

    )

  var-state
  (-apply-child-change-command [node-state graph-manager-value observed changes resolver worklist]
    (if-some [observed-by (not-empty (get-in graph-manager-value [:observed-by observed]))]
      (if-some [old-state (-> graph-manager-value :node-state (get observed))]
        graph-manager-value
        (let [{:keys [change-focus]} worklist]
          (run!
            (fn [parent]
              (update!+- change-focus parent add-dep observed))
            (get node-state :observes))
          (-> graph-manager-value
              (assoc-in [:node-state observed] node-state)
              (update :observed-by update-observed-by observed
                (into {}
                  (map (fn [dtor] [dtor true]))
                  (get node-state :observes))))))
      (->
        (if-some [observes (not-empty (get node-state :observes))]
          (let [{:keys [change-focus]} worklist]
            (run!
              (fn [parent]
                (update!+- change-focus parent rm-dep observed))
              observes)
            (-> graph-manager-value
                (update :observed-by update-observed-by observed (into {} (map (fn [x] [x false])) (get node-state :observes)))
                ))
          graph-manager-value)
        (update :graph-value dissoc observed)
        (update :node-state dissoc observed)
        ))
    ))

#_(defn apply-child-change-commands [graph-manager-value resolver child changes worklist-atom dirty-curators]
  (reduce-kv
    (fn [graph-manager-value parent added|removed]
      (-apply-child-change-command (get-init-node graph-manager-value resolver parent worklist)
        graph-manager-value child parent added|removed resolver worklist-atom
        dirty-curators))
    graph-manager-value
    changes))

(defn apply-focus-changes [graph-manager-value resolver focus-changes worklist]
  (reduce-kv
    (fn [graph-manager-value observed changes]
      (-apply-child-change-command (get-init-node graph-manager-value resolver observed worklist)
        graph-manager-value observed  changes resolver worklist))
    graph-manager-value
    focus-changes))

#_(defn propagate-dependency-changes [graph-manager-value resolver descriptor changes worklist-atom dirty-curators]
  (apply-child-change-commands
    (-> graph-manager-value
        (update :observed-by update-observed-by descriptor changes))
    resolver
    descriptor
    changes
    worklist-atom
    dirty-curators))

;(s/fdef -apply-command
;  :args (s/cat
;          :curator-state  ::curator-proto/curator-state
;          :curator any?
;          :command vector?
;          :graph-value ::graph-value
;          :observed-by (s/coll-of ::descriptor)
;          :observes (s/coll-of ::descriptor))
;  :ret ::curator-proto/curator-state)

(defn finalize-tx [{node :node :as node-state} graph-value resolver descriptor]
  (if-some [finalize (::curator-proto/finalize (:dtor-impl node-state))]
    (assoc node-state :node (finalize descriptor graph-value node))
    node-state))

(defn assert-valid-finalized-node-state [{{:keys [change-focus set-projections] } :node} descriptor-name]
  (assert (empty? change-focus) descriptor-name)
  (assert (empty? set-projections) descriptor-name))


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
          (assoc-in
            graph-manager-value
            [:node-state
             descriptor]
            (remove-effects new-state))))
      graph-manager-value
      disturbed-curators)))

(defn apply-effects
  [graph-manager sync-effects async-effects]
  (let [scheduler (.-scheduler graph-manager)]
    (g/-run-sync scheduler graph-manager sync-effects)
    (g/-run-async scheduler graph-manager async-effects)))

;(s/fdef apply-command
;  :args (s/cat
;          :graph-manager-value  ::graph-manager-value
;          :curator any?
;          :command vector?)
;  :ret ::graph-manager-value)
(def recursion-limit 1000000)

(defn -apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver descriptor command worklist]
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
            node-state   (get-init-node graph-manager-value resolver descriptor worklist)
            n (:node node-state)]
        (assert (instance? curator-state node-state) (pr-str node-state))
        (assoc-in graph-manager-value [:node-state descriptor]
          (assoc node-state :node
                            (if-some [apply-command (::curator-proto/apply-command dtor-impl)]
                              (apply-command descriptor graph-value
                                (tx-init-curator n graph-manager-value descriptor worklist)
                                command)
                              (assert false)))))
      :hitch2.descriptor.kind/var
      (-apply-command graph-manager-value resolver (get-curator dtor-impl descriptor)
                     command worklist))))

(defn apply-command
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver descriptor command sync-effects-atom async-effects-atom]
  (let [work-list (volatile! (make-work-list))
        graph-manager-value (-apply-command graph-manager-value resolver descriptor command @work-list)

        graph-manager-value (init-worklist graph-manager-value work-list)
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              work-list
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! (:touched-curators @work-list))
      sync-effects-atom async-effects-atom)
    ))

(defn apply-commands
  "Apply command to curator and then allow the graph to settle. Returns
  the new graph manager value."
  [graph-manager-value resolver cmds sync-effects-atom async-effects-atom]
  (let [work-list (volatile! (make-work-list))
        work-listv @work-list
        graph-manager-value
                           (reduce
                             (fn [gmv [descriptor command]]
                               (-apply-command gmv resolver descriptor command work-listv))
                             graph-manager-value
                             cmds)

        graph-manager-value (init-worklist graph-manager-value work-list)
        graph-manager-value (propagate-changes graph-manager-value
                              resolver
                              work-list
                              recursion-limit)]
    (finalize-effects graph-manager-value
      resolver
      (persistent! (:touched-curators @work-list))
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
    (let [node-state (get-in @state [:node-state descriptor] NOT-IN-GRAPH-SENTINEL)]
      (if (identical? node-state NOT-IN-GRAPH-SENTINEL)
        NOT-IN-GRAPH-SENTINEL
        (:observes node-state))))
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


