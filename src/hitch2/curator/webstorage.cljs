(ns hitch2.curator.webstorage
  (:require [clojure.edn :as edn]
            [hitch2.graph :as hitch]
            [hitch2.def.curator :as curator]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec :refer-macros [def-descriptor-spec]]
            [hitch2.descriptor-impl-registry :as reg
             :refer-macros [def-registered-descriptor]]
            goog.cssom))

(defn- init-update-count [m k]
  (if (contains? m k)
    m
    (assoc m k 0)))

(defn- get-storage
  ([storage k]
   (get-storage storage k nil))
  ([storage k nf]
   (if-some [v (.getItem storage k)]
     (edn/read-string v)
     nf)))

(defn storage-getter [storage]
  (reify
    ILookup
    (-lookup [_ k]
      (get-storage storage k))
    (-lookup [_ k nf]
      (get-storage storage k nf))))

(declare storage-info)

(defn gm->handler
  [gm {{:keys [storage]} :term :as machine-selector}]
  (fn [storageEvent]
    (let [storageArea (.-storageArea storageEvent)]
      (when (identical? storageArea (-> storage-info storage :storage/underlying))
        (hitch/apply-commands gm [[machine-selector [::change (.-key storageEvent)]]])))))

(defmethod graph-proto/run-effect ::subscribe
  [gm {:keys [curator-selector]}]
  (let [event-handler (gm->handler gm curator-selector)]
    (.addEventListener goog/global "storage" event-handler)
    (hitch/apply-commands gm [[curator-selector [::subscription-fn event-handler]]])))

(defmethod graph-proto/run-effect ::unsubscribe
  [gm {:keys [subscription-fn]}]
  (.removeEventListener goog/global "storage" subscription-fn))

(defmethod graph-proto/run-effect ::assoc
  [gm {:keys [storage key value]}]
  (.setItem (-> storage-info storage :storage/underlying) key (pr-str value)))

(def-descriptor-spec storage-machine-spec
  :curator
  :canonical-form :map
  :positional-params [:storage])

(def machine-impl
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/curator
   ::curator/init
   (fn [{{:keys [storage]} :term :as curator-selector}]
     (update curator/initial-curator-state
       ;; node must remember the
       ;; subscription function is all.
       :async-effects
       conj
       {:type             ::subscribe
        :curator-selector curator-selector}))
   ;; todo: what is the return of this? still prototyping
   ::curator/deinit
   (fn [machine-selector]
     (fn [node]
       (let [subscription-fn (:state node)]
         (update node :async-effects conj
           {:type            ::unsubscribe
            :subscription-fn subscription-fn}))))
   ::curator/curation-changes
   (fn [{{:keys [storage]} :term :as machine-selector}]
     (fn [gmv node children-added children-removed]
       (cond-> node
         (not-empty children-added)
         (update :set-projections
           into
           (map (fn [{{:keys [key]} :term :as sel}]
                  [sel (get-storage (-> storage-info storage :storage/underlying) key)]))
           children-added))))
   ::curator/apply-command
   (fn [{{:keys [storage]} :term :as machine-selector}]
     (fn [graph-value node command]
       (let [[cmd arg] command]
         (case cmd
           ;; [:set-value indexed]
           ::subscription-fn
           (assoc node :state arg)

           ::change
           (let [k               arg
                 var-constructor (-> storage-info storage :storage/var)
                 value           (get-storage (-> storage-info storage :storage/underlying) k)]
             (update node
               :set-projections
               assoc (var-constructor k) value))

           ::assoc (let [[_ k v]         command
                         var-constructor (-> storage-info storage :storage/var)]
                     (-> node
                       (update :async-effects
                         conj
                         {:type    ::assoc
                          :storage storage
                          :key     k
                          :value   v})
                       (update :set-projections
                         assoc (var-constructor k) v)))))))})


(reg/def-registered-descriptor storage-machine-spec' storage-machine-spec machine-impl)

(def localstorage-machine (hitch/positional-dtor storage-machine-spec' ::local))
(def sessionstorage-machine (hitch/positional-dtor storage-machine-spec' ::session))

(def-descriptor-spec localstorage-spec
  :not-machine
  :canonical-form :map
  :positional-params [:key])

(def localstorage-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (constantly localstorage-machine)})

(reg/def-registered-descriptor localstorage-spec' localstorage-spec localstorage-impl)

(defn localstorage [k] (hitch/->dtor localstorage-spec' {:key k}))

(def-descriptor-spec sessionstorage-spec
  :not-machine
  :canonical-form :map
  :positional-params [:key])

(def sessionstorage-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (constantly sessionstorage-machine)})

(reg/def-registered-descriptor sessionstorage-spec' sessionstorage-spec sessionstorage-impl)

(defn sessionstorage [k] (hitch/->dtor sessionstorage-spec' {:key k}))

(def storage-info {::local   {:storage/underlying js/localStorage
                              :storage/var        localstorage}
                   ::session {:storage/underlying js/sessionStorage
                              :storage/var        sessionstorage}})
