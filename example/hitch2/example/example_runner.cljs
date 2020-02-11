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
            [hitch2.process-manager :as pm]
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
            ["@material-ui/core/Paper" :default Paper])
  (:import [goog.async nextTick]))


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

(defn update-var-state [children-added children-removed]
  (fn [state]
    (assoc state :vars
     (-> (reduce disj
           (get state :vars)
           children-removed)
       (into children-added)))))

(defn dtor->key [dtor]
  (case (-> dtor :name)
    hitch2.example.example-runner/address-line-spec :address-line
    hitch2.example.example-runner/address-line-stored-spec :address-line

    hitch2.example.example-runner/city-line-spec :city-line
    hitch2.example.example-runner/city-line-stored-spec :city-line

    hitch2.example.example-runner/state-line-spec :state-line
    hitch2.example.example-runner/state-line-stored-spec :state-line
    
    hitch2.example.example-runner/zip-line-spec :zip-line
    hitch2.example.example-runner/zip-line-stored-spec :zip-line))

(defn value-getter [value dtor]
  (get value (dtor->key dtor)))

(def local-storage-curator?
  #{`address-line-stored-spec'})
(def transient-state-curator?
  #{`address-line-spec'})

(def-descriptor-spec local-store-proc-spec :process)
(def local-store-proc-dtor (hitch/->dtor local-store-proc-spec nil))

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
                                                              (-> n
                                                                (update :state assoc :local-storage local-storage-value)
                                                                (update :state assoc :transient-input local-storage-value)))))
                                                  node
                                                  parent-descriptors)]
                                            (update updated-node
                                              :set-projections
                                              into
                                              (map (fn [added]
                                                     [added (value-getter (-> updated-node :state :local-storage) added)]))
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
             (keep (fn [added]
                     (let [descriptor-name (:name added)]
                       (if (local-storage-curator? descriptor-name)
                         [added (value-getter (-> updated-node :state :transient-input) added)]
                         [added (-> updated-node :state :transient-input)]))))
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
           :submit-item (let [value arg
                              dtor arg2
                              new-state (into (-> node :state :local-storage) {(dtor->key dtor) value})]
                         (update-in node [:outbox local-store-proc-dtor]
                           (fnil conj [])
                           {:commands [[storage [::store/assoc :curator-storage-form-example new-state]]]}))
           :clear (update-in node [:outbox local-store-proc-dtor]
                    (fnil conj [])
                    {:commands [[storage [::store/assoc :curator-storage-form-example {}]]]})
           :submit (update-in node [:outbox local-store-proc-dtor]
                         (fnil conj [])
                         {:commands [[storage [::store/assoc :curator-storage-form-example (-> node :state :transient-input)]]]})))))})

(reg/def-registered-descriptor address-form-machine-spec' address-form-machine-spec address-form-machine-impl)

(def local-store-proc-impl
    {:hitch2.descriptor.impl/kind
     :hitch2.descriptor.kind/process
     ;; process manager/create
     ::pm/create (fn [pdtor] ;; process-descriptor
                   (reify pm/IProcess
                     (-send-message! [process {:keys [graph-value gm commands] :as effect}] ;; graph-value & gm inserted by `effect`
                       (nextTick (fn [] (hitch/apply-commands gm commands))))
                     (-kill-process! [process] true)))})
(reg/def-registered-descriptor local-store-proc-spec' local-store-proc-spec local-store-proc-impl)

;;;;;;; address line
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

(def-descriptor-spec address-line-stored-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def address-line-stored-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))})
(reg/def-registered-descriptor address-line-stored-spec' address-line-stored-spec address-line-stored-impl)
(defn address-line-stored [keyspace]
  (hitch/->dtor address-line-stored-spec' {:keyspace keyspace}))

;;;;;;;;;;;; city 
(def-descriptor-spec city-line-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def city-line-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))}) ;; ->dtor pronounced "create descriptor"
(reg/def-registered-descriptor city-line-spec' city-line-spec city-line-impl)
(defn city-line [keyspace]
  (hitch/->dtor city-line-spec' {:keyspace keyspace}))

(def-descriptor-spec city-line-stored-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def city-line-stored-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))})
(reg/def-registered-descriptor city-line-stored-spec' city-line-stored-spec city-line-stored-impl)
(defn city-line-stored [keyspace]
  (hitch/->dtor city-line-stored-spec' {:keyspace keyspace}))

;;; state
(def-descriptor-spec state-line-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def state-line-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))}) ;; ->dtor pronounced "create descriptor"
(reg/def-registered-descriptor state-line-spec' state-line-spec state-line-impl)
(defn state-line [keyspace]
  (hitch/->dtor state-line-spec' {:keyspace keyspace}))

(def-descriptor-spec state-line-stored-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def state-line-stored-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))})
(reg/def-registered-descriptor state-line-stored-spec' state-line-stored-spec state-line-stored-impl)
(defn state-line-stored [keyspace]
  (hitch/->dtor state-line-stored-spec' {:keyspace keyspace}))

;;; zip
(def-descriptor-spec zip-line-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def zip-line-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))}) ;; ->dtor pronounced "create descriptor"
(reg/def-registered-descriptor zip-line-spec' zip-line-spec zip-line-impl)
(defn zip-line [keyspace]
  (hitch/->dtor zip-line-spec' {:keyspace keyspace}))

(def-descriptor-spec zip-line-stored-spec
  :not-machine
  :canonical-form :map
  :positional-params [:keyspace])
(def zip-line-stored-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (hitch/->dtor address-form-machine-spec'
                                           {:keyspace (-> descriptor :term :keyspace)}))})
(reg/def-registered-descriptor zip-line-stored-spec' zip-line-stored-spec zip-line-stored-impl)
(defn zip-line-stored [keyspace]
  (hitch/->dtor zip-line-stored-spec' {:keyspace keyspace}))


(defn numeric? [str-val] (-> str-val (clojure.string/split " ") first (->> (re-matches #"\d+")) nil? not))
(def states
 {:AL "Alabama" :AK "Alaska" :AZ "Arizona" :AR "Arkansas" :CA "California" :CO "Colorado" :CT "Connecticut"
  :DE "Delaware" :DC "District of Columbia" :FL "Florida" :GA "Georgia" :HI "Hawaii" :ID "Idaho" :IL "Illinois"
  :IN "Indiana" :IA "Iowa" :KS "Kansas" :KY "Kentucky" :LA "Louisiana" :ME "Maine" :MD "Maryland" :MA "Massachusetts"
  :MI "Michigan" :MN "Minnesota" :MS "Mississippi" :MO "Missouri" :MT "Montana" :NE "Nebraska" :NV "Nevada" :NH "New Hampshire"
  :NJ "New Jersey" :NM "New Mexico" :NY "New York" :NC "North Carolina" :ND "North Dakota" :OH "Ohio" :OK "Oklahoma"
  :OR "Oregon" :PA "Pennsylvania" :PR "Puerto Rico" :RI "Rhode Island" :SC "South Carolina" :SD "South Dakota"
  :TN "Tennessee" :TX "Texas" :UT "Utah" :VT "Vermont" :VA "Virginia" :WA "Washington" :WV "West Virginia" :WI "Wisconsin"
  :WY "Wyoming"})

(defn changes-popper [{:keys [graph field-transient-val field-stored-val field-dtor anchor-elem] :as props}]
  (RE Popper {:open      (not= field-transient-val field-stored-val)
              :anchorEl  (.-current anchor-elem)
              :placement "right-end"
              ;; :modifiers #js  {"arrow" {"enabled" true "element" "arrowRef"}}
              }
    (d/div {:style #js {"border"          "1px solid black"
                        "backgroundColor" "white"}}
      (d/div {} (str "Current Value:  " field-stored-val))
      (d/div {} (str "New Value:  " field-transient-val))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true} (RE Button {:onClick (fn [e]
                                                     (hitch/apply-commands graph
                                                       [[field-dtor [:value-change field-stored-val field-dtor]]]))}
                                "Discard"))
        (RE Grid {:item true} (RE Button {:onClick (fn [e]
                                                     (hitch/apply-commands graph
                                                       [[field-dtor [:submit-item field-transient-val field-dtor]]]))}
                                "Save"))))))

(defn curator-storage-form [{:keys [graph] :as props}]
  (let [address-line-dtor        (address-line :curator-storage-form-example)
        address-line-val         (hitch-hook/useSelected address-line-dtor)
        address-line-stored-dtor (address-line-stored :curator-storage-form-example)
        address-line-stored-val  (hitch-hook/useSelected address-line-stored-dtor)

        city-line-dtor (city-line :curator-storage-form-example)
        city-line-val  (hitch-hook/useSelected city-line-dtor)
        city-line-stored-dtor (city-line-stored :curator-storage-form-example)
        city-line-stored-val (hitch-hook/useSelected city-line-stored-dtor)

        state-line-dtor (state-line :curator-storage-form-example)
        state-line-val  (hitch-hook/useSelected state-line-dtor)
        state-line-stored-dtor (state-line-stored :curator-storage-form-example)
        state-line-stored-val (hitch-hook/useSelected state-line-stored-dtor)
        
        zip-line-dtor (zip-line :curator-storage-form-example)
        zip-line-val  (hitch-hook/useSelected zip-line-dtor)
        zip-line-stored-dtor (zip-line-stored :curator-storage-form-example)
        zip-line-stored-val (hitch-hook/useSelected zip-line-stored-dtor)
        
        addressLineR (react/useRef)
        cityLineR (react/useRef)
        stateLineR (react/useRef)
        zipLineR (react/useRef)]
    (RE Paper {:style #js {"border"  "1px solid black"
                           "padding" "10px"}}
      (if (hitch-hook/loaded? address-line-val)
        (RE Paper {}
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Address line"
                             :inputRef addressLineR
                             :error (not (numeric? address-line-val))
                             :onChange (fn [e]
                                         (hitch/apply-commands graph [[address-line-dtor [:value-change (.. e -target -value) address-line-dtor]]]))
                             :value (or address-line-val "")})
              (when (.-current addressLineR)
                (CE changes-popper {:field-transient-val address-line-val
                                    :field-stored-val address-line-stored-val
                                    :field-dtor address-line-dtor
                                    :anchor-elem addressLineR
                                    :graph graph}))))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "City"
                             :inputRef cityLineR
                             :onChange (fn [e]
                                         (hitch/apply-commands graph [[city-line-dtor [:value-change (.. e -target -value) city-line-dtor]]]))
                             :value (or city-line-val "")})
              (when (.-current cityLineR)
                (CE changes-popper {:field-transient-val city-line-val
                                    :field-stored-val city-line-stored-val
                                    :field-dtor city-line-dtor
                                    :anchor-elem cityLineR
                                    :graph graph}))))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "State"
                             :error (false? (contains? (-> states vals set) state-line-val))
                             :inputRef stateLineR
                             :onChange (fn [e]
                                         (hitch/apply-commands graph [[state-line-dtor [:value-change (.. e -target -value) state-line-dtor]]]))
                             :value (or state-line-val "")})
              (when (.-current stateLineR)
                (CE changes-popper {:field-transient-val state-line-val
                                    :field-stored-val    state-line-stored-val
                                    :field-dtor          state-line-dtor
                                    :anchor-elem         stateLineR
                                    :graph               graph}))))
          (RE Grid {:container true :spacing 2}
            (RE Grid {:item true}
              (RE TextField {:label        "Zip"
                             :inputRef zipLineR
                             :onChange (fn [e]
                                         (hitch/apply-commands graph [[zip-line-dtor [:value-change (.. e -target -value) zip-line-dtor]]]))
                             :value (or zip-line-val "")})
              (when (.-current zipLineR)
                (CE changes-popper {:field-transient-val zip-line-val
                                    :field-stored-val    zip-line-stored-val
                                    :field-dtor          zip-line-dtor
                                    :anchor-elem         zipLineR
                                    :graph               graph})))))
        (d/div {} "loading ..."))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn [] (hitch/apply-commands graph [[address-line-dtor [:clear {}]]])))}
            "clear"))
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn [] (hitch/apply-commands graph [[address-line-dtor [:submit {}]]])))}
            "submit"))))))

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
