(ns hitch2.example.example-runner
  (:require [crinkle.component :refer [RE CE] :as c]
            [crinkle.dom :as d]
            ["react-dom" :refer [render]]
            ["react" :as react]
            [hitch2.graph :as hitch]
            [hitch2.curator.webstorage :as store]
            [hitch2.graph-manager.atom :as atom-gm]
            [hitch2.def.curator :as curator]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.spec :refer-macros [def-descriptor-spec]]
            [hitch2.descriptor-impl-registry :as reg
             :refer [registry-resolver]]
            [hitch2.protocols.graph-manager :as graph-proto]
            react-hitch.curator.react-hook
            react-hitch.qui-tracker
            [react-hitch.hooks :as hitch-hook]
            [react-hitch.graph :as hitch-graph]

            ["@material-ui/core/Popper" :default Popper]            
            ["@material-ui/core/Button" :default Button]
            ["@material-ui/core/Grid" :default Grid]
            ["@material-ui/core/TextField" :default TextField]
            ["@material-ui/core/Select" :default Select]
            ["@material-ui/core/InputLabel" :default InputLabel]
            ["@material-ui/core/MenuItem" :default MenuItem]
            ["@material-ui/core/Paper" :default Paper]))


(def storage store/localstorage-machine)

(defn minimal-storage-form [{:keys [graph] :as props}]
  (let [ov (hitch-hook/useSelected (store/localstorage :minimal-example))]
    (d/div {:style #js {"border" "1px solid black"
                        "padding" "10px"}}
      (d/span {} (pr-str ov))
      (RE Button {:style   #js {:margin "10px"}
                  :onClick (react/useCallback
                             (fn [] (hitch/apply-commands graph [[storage [::store/assoc :minimal-example (if ov (inc ov) 0)]]]))
                             #js [graph ov])} "add")
      (RE Button {:style   #js {:margin "10px"}
                  :onClick (react/useCallback
                             (fn [] (hitch/apply-commands graph [[storage [::store/assoc :minimal-example 0]]]))
                             #js [graph ov])} "reset"))))

(defn minimal-example [{:keys [] :as props}]
  (let [[graph setGraph] (react/useState (fn [] (atom-gm/make-gm registry-resolver)))]
    (d/div {}
     (RE hitch-graph/GraphContext-Provider {:value graph}
       (CE minimal-storage-form {:graph graph})))))

(defn basic-storage-form [{:keys [graph] :as props}]
  (let [ov (hitch-hook/useSelected (store/localstorage :basic-form-example))
        address-line-ref (react/useRef)
        city-line-ref (react/useRef)
        state-line-ref (react/useRef)
        zip-line-ref (react/useRef)]
    (RE Paper {:style #js {"border"  "1px solid black"
                           "padding" "10px"}}
      (if (hitch-hook/loaded? ov)
        (RE Paper {}
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Address line"
                             :inputRef     address-line-ref
                             :defaultValue (get ov :address-line)})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "City"
                             :inputRef     city-line-ref
                             :defaultValue (get ov :city-line)})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "State"
                             :inputRef     state-line-ref
                             :defaultValue (:state-line ov)})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Zip"
                             :inputRef     zip-line-ref
                             :defaultValue (:zip-line ov)}))))
        (d/div {} "loading ..."))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn []
                                   (set! (.. address-line-ref -current -value) "")
                                   (set! (.. city-line-ref -current -value) "")
                                   (set! (.. state-line-ref -current -value) "")
                                   (set! (.. zip-line-ref -current -value) "")
                                   (hitch/apply-commands graph [[storage [::store/assoc :basic-form-example {}]]]))
                                 #js [graph ov])} "clear"))
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn [] 
                                   (hitch/apply-commands graph
                                     [[storage [::store/assoc :basic-form-example
                                                {:address-line (.. address-line-ref -current -value)
                                                 :city-line    (.. city-line-ref -current -value)
                                                 :state-line   (.. state-line-ref -current -value)
                                                 :zip-line     (.. zip-line-ref -current -value)}]]]))
                                 #js [graph ov])} "submit"))))))

(defn raw-form-example [{:keys [] :as props}]
  (let [[graph setGraph] (react/useState (fn [] (atom-gm/make-gm registry-resolver)))]
    (d/div {:style #js {"border" "1px solid black"
                        "padding" "10px"}}
     (RE hitch-graph/GraphContext-Provider {:value graph}
       (CE basic-storage-form {:graph graph})))))


(def-descriptor-spec address-form-machine-spec
  :curator
  :canonical-form :map
  :positional-params [:keyspace])
(def address-form-machine-impl
  {:hitch2.descriptor.impl/kind     :hitch2.descriptor.kind/curator
   ::curator/init
   (fn [machine-selector]
     (prn :change-focus {(store/localstorage (-> machine-selector :term :keyspace)) true})
     (assoc curator/initial-curator-state
       :state {:local-storage :not-loaded
               :transient-input {}
               :vars #{}}
       :change-focus {(store/localstorage (-> machine-selector :term :keyspace)) true}))
   ::curator/observed-value-changes (fn [machine-selector]
                                      (fn [graph-manager-value node parent-descriptors]
                                        (let [graph-value (graph-proto/-graph-value graph-manager-value)]
                                          (let [updated-node
                                                (reduce (fn [n dtor]
                                                          (let [local-storage-value (get graph-value dtor :not-loaded)]
                                                            (if (= :not-loaded local-storage-value)
                                                              n
                                                              (update n :state assoc :local-storage local-storage-value))))
                                                  node
                                                  parent-descriptors)]
                                            (update updated-node
                                              :set-projections
                                              into
                                              (map (fn [added]
                                                     [added (value-getter (-> updated-node :state :transient-input) added)]))
                                              (get-in updated-node [:state :vars]))))))
   ::curator/curation-changes
   (fn [machine-selector]
     (fn [gmv node children-added children-removed]
       (let [updated-node (update node :state (update-var-state children-added children-removed))]
         (if (= :not-loaded (-> node :state :local-storage))
           updated-node
           (update updated-node
             :set-projections
             into
             (map (fn [added]
                    [added (value-getter (-> updated-node :state :transient-input) added)]))
             children-added)))))
   ::curator/apply-command
   (fn [machine-selector]
     (fn [gmv node command]
       (let [[cmd arg arg2] command]
         (case cmd
           :value-change (let [value arg
                               dtor arg2]
                           (-> node
                             (update :set-projections assoc dtor value)
                             (update-in [:state :transient-input] assoc (dtor->key dtor) value)))
           :submit [] ; commit to local storage
           ))))})

(reg/def-registered-descriptor address-form-machine-spec' address-form-machine-spec address-form-machine-impl)

(def-descriptor-spec address-line-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def address-line-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))}) ;; ->dtor pronounced "create descriptor"
(reg/def-registered-descriptor address-line-spec' address-line-spec address-line-impl)
(defn address-line [keyspace]
  (hitch/->dtor address-line-spec' {:keyspace keyspace}))

(defn curator-storage-form [{:keys [graph] :as props}]
  (let [ov (hitch-hook/useSelected (store/localstorage :basic-form-example))
        [addressLine setAddressLine] (react/useState "")
        [cityLine setCityLine] (react/useState "")
        [stateLine setStateLine] (react/useState "")
        [zipLine setZipLine] (react/useState "")]
    (RE Paper {:style #js {"border"  "1px solid black"
                           "padding" "10px"}}
      (if (hitch-hook/loaded? ov)
        (RE Paper {}
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Address line"
                             ;; :InputProps #js {:onKeyDown (fn [e] 
                             ;;                               (let [kc (.. e -keyCode)]
                             ;;                                 (.log js/console kc)
                             ;;                                 (setAddressLine (fn [v] (str v (char kc)))))
                             ;;                            )}
                             :onChange (fn [e] (.log js/console (.. e -target -value)) (setAddressLine (.. e -target -value)))
                             :value addressLine})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "City"
                             :value (get ov :city-line "")})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "State"
                             :value (get ov :state-line "")})))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Zip"
                             :value (get ov :zip-line "")}))))
        (d/div {} "loading ..."))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      ;; :onClick (react/useCallback
                      ;;            (fn []
                      ;;              (set! (.. address-line-ref -current -value) "")
                      ;;              (set! (.. city-line-ref -current -value) "")
                      ;;              (set! (.. state-line-ref -current -value) "")
                      ;;              (set! (.. zip-line-ref -current -value) "")
                      ;;              (hitch/apply-commands graph [[storage [::store/assoc :basic-form-example {}]]]))
                      ;;            #js [graph ov])
                      } "clear"))
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      ;; :onClick (react/useCallback
                      ;;            (fn [] 
                      ;;              (hitch/apply-commands graph
                      ;;                [[storage [::store/assoc :basic-form-example
                      ;;                           {:address-line (.. address-line-ref -current -value)
                      ;;                            :city-line    (.. city-line-ref -current -value)
                      ;;                            :state-line   (.. state-line-ref -current -value)
                      ;;                            :zip-line     (.. zip-line-ref -current -value)}]]]))
                      ;;            #js [graph ov])
                      } "submit"))))))

(defn curator-form-example [{:keys [] :as props}]
  (let [[graph setGraph] (react/useState (fn [] (atom-gm/make-gm registry-resolver)))]
    (d/div {:style #js {"border" "1px solid black"
                        "padding" "10px"}}
     (RE hitch-graph/GraphContext-Provider {:value graph}
       (CE curator-storage-form {:graph graph})))))

(defn -main [& args]
  (render
    (d/div {}
      (CE minimal-example {})
      (CE raw-form-example {})
      (CE curator-form-example {}))
    (.. js/document (getElementById "app"))))
