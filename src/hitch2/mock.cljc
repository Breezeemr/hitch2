(ns hitch2.mock
  (:require [hitch2.descriptor-impl-registry :as reg
             :refer [get-descriptor-impl]]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [cognitect.anomalies :as ca]
            [hitch2.def.curator :as curator-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.descriptor :as descriptor]))

(def-descriptor-spec mock-curator-spec
  :curator
  :canonical-form :map)

(defn add-or-dissoc [col k v]
  (if (identical? v NOT-FOUND-SENTINEL)
    (dissoc col k)
    (assoc col k v)))

(defn add-or-dissoc-all [col changes]
  (reduce-kv
    add-or-dissoc
    col
    changes))

(def initial-node (assoc curator-proto/initial-curator-state :state {}))

(def mock-curator-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/curator
   ::curator-proto/init         (fn [curator-descriptor]
                                  initial-node)
   ::curator-proto/curation-changes
                                (fn [curator-descriptor graph-value node children-added children-removed]
                                  (assoc node
                                    :set-projections
                                    (-> (:set-projections node)
                                        (into
                                          (keep
                                            (fn [x]
                                              (let [val (-> node :state (get x NOT-FOUND-SENTINEL))]
                                                (when-not (identical? val NOT-FOUND-SENTINEL)
                                                  [x val]))))
                                          children-added))))
   ::curator-proto/apply-command
                                (fn [curator-descriptor graph-value node command]
                                  (case (nth command 0)
                                    :set-value (let [[_ dtor val] command]
                                                 (-> node
                                                     (update :state add-or-dissoc dtor val)
                                                     (update :set-projections assoc dtor val)))
                                    :set-values
                                    (let [[_ changes] command]
                                      (-> node
                                          (update :state add-or-dissoc-all changes)
                                          (update :set-projections into changes)))
                                    :clear
                                    (-> node
                                        (assoc :state {})
                                        (update :set-projections
                                          into (map (fn [[k v]] [k NOT-FOUND-SENTINEL]))
                                          (-> node :state)))
                                    (prn command)))})

(reg/def-registered-descriptor mock-curator mock-curator-spec mock-curator-impl)

(def mock-curator-instance
  (descriptor/->dtor mock-curator nil))



(def mock-var-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator
                                (fn [sel]
                                  mock-curator-instance)})


(defn mock-registry-resolver
  "A resolver that takes a whitelist that looks up descriptor implementations dynamically from the
  mutable registry or throws if unavailable. If not on the whitelist it uses a mocked implementation"
  [var-curator-whitelist halting-override]
  (fn
    [descriptor]
    (let [sname (:name descriptor)
          {:keys [:hitch2.descriptor.impl/kind] :as impl}
          (get-descriptor-impl sname)]
      (when (nil? impl) (throw (ex-info (str "Could not find implementation in descriptor registry " sname)
                                 {::ca/category    ::ca/not-found
                                  :descriptor-name sname
                                  :descriptor      descriptor})))
      (if (var-curator-whitelist sname)
        impl
        (case kind
          :hitch2.descriptor.kind/halting
          (if (halting-override sname)
            mock-var-impl
            impl)
          :hitch2.descriptor.kind/curator
          impl
          :hitch2.descriptor.kind/var
          impl
          :hitch2.descriptor.kind/process
          impl)))))
