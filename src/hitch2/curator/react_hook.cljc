(ns hitch2.curator.react-hook
  (:require [hitch2.protocols.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]
            [hitch2.selector-impl-registry :as reg]
            [clojure.set :as set]))

#_(def react-hook-spec
  {:hitch.selector/name      ::react-hook
   :hitch.selector.spec/kind :machine})

(def initial-node
  (assoc machine-proto/initial-curator-state
    :state {:rc->sel  {}
            :sel->rc  {}
            :dirty-rc #{}}))

(defn remove-called-hooks [state selectors]
  (reduce dissoc state selectors))

(def-selector-spec react-hook-spec
  :machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/positional)

(defn reset-component-parents [node rc new-parents]
  (let [state               (:state node)
        rc->sel             (:rc->sel state)
        sel->rc             (:sel->rc state)
        old-parents         (get rc->sel rc #{})
        del-parents         (set/difference old-parents new-parents)
        add-parents         (set/difference new-parents old-parents)
        rc->sel'            (assoc rc->sel rc new-parents)
        shared-parent-delta (volatile! {})
        sel->rc'            (as-> sel->rc <>
                              (reduce
                                (fn [sel->rc sel]
                                  (when (nil? (get sel->rc sel))
                                    (vswap! shared-parent-delta assoc sel true))
                                  (update sel->rc sel (fnil conj #{}) rc))
                                <> add-parents)
                              (reduce
                                (fn [sel->rc sel]
                                  (let [rcs        (get sel->rc sel #{})
                                        rcs'       (disj rcs rc)
                                        went-away? (and (pos? (count rcs)) (zero? (count rcs')))]
                                    (when went-away?
                                      (vswap! shared-parent-delta assoc sel false))
                                    (if went-away?
                                      (dissoc sel->rc sel)
                                      (assoc sel->rc sel rcs'))))
                                <> del-parents))
        state'              (assoc state :rc->sel rc->sel'
                                         :sel->rc sel->rc')]
    (-> node
        (assoc :state state')
        (update :change-focus into @shared-parent-delta))))

(def react-hook-impl
  {:hitch.selector.impl/kind
   :hitch.selector.kind/machine

   ::machine-proto/init
   (fn [machine-selector] initial-node)

   ::machine-proto/apply-command
   (fn [machine-selector graph-value node command]
     (case (nth command 0)
       :reset-component-parents
       (let [[_ rc new-parents] command]
         (reset-component-parents node rc new-parents))))

   ::machine-proto/observed-value-changes
   (fn [machine-selector graph-value node parent-selectors]
     (let [sel->rc   (-> node :state :sel->rc)
           dirty-rc  (-> node :state :dirty-rc)
           dirty-rc' (transduce
                       (map sel->rc)
                       into dirty-rc parent-selectors)]
       (assoc-in node [:state :dirty-rc] dirty-rc')))

   ::machine-proto/finalize
   (fn [_ graph-value node]
     (let [dirty-rc (-> node :state :dirty-rc)]
       (cond-> (assoc-in node [:state :dirty-rc] #{})
         (pos? (count dirty-rc))
         (update :async-effects conj
           {:type       :rerender-components
            :components dirty-rc}))))})

(reg/def-registered-selector Rreact-hook react-hook-spec react-hook-impl)

(def react-hooker (sel-proto/sel Rreact-hook))
