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
  (-> dtor :term :path second))

(defn value-getter [value dtor]
  (get-in value (-> dtor :term :path)))

(defn local-storage-descriptor? [dtor]
  (let [[type field] (-> dtor :term :path)]
    (= type :localstorage)))

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
     (assoc curator/initial-curator-state
       :state {:local-storage   :not-loaded
               :transient-input {}
               :vars            #{}}
       :change-focus {(store/localstorage (-> machine-selector :term :keyspace)) true}))
   ::curator/observed-value-changes (fn [machine-selector]
                                      (fn [graph-manager-value node parent-descriptors]
                                        (let [graph-value                        (graph-proto/-graph-value graph-manager-value)
                                              previous-local-storage-not-loaded? (= :not-loaded (-> node :state :local-storage))
                                              updated-node
                                              (reduce (fn [n dtor]
                                                        (let [local-storage-value (get graph-value dtor :not-loaded)]
                                                          (if (= :not-loaded local-storage-value)
                                                            n
                                                            (if previous-local-storage-not-loaded?
                                                              (-> n
                                                                (update :state assoc :local-storage local-storage-value)
                                                                (update :state assoc :transient-input local-storage-value))
                                                              (update n :state assoc :local-storage local-storage-value)))))
                                                node
                                                parent-descriptors)]
                                          
                                          (update updated-node
                                            :set-projections
                                            into
                                            (keep (fn [added]
                                                    (if (local-storage-descriptor? added)
                                                      [added (get (-> updated-node :state :local-storage) (dtor->key added))]
                                                      (when previous-local-storage-not-loaded?
                                                        [added (get (-> updated-node :state :local-storage) (dtor->key added))]))))
                                            (get-in updated-node [:state :vars])))))
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
                     [added (value-getter (-> updated-node :state) added)]))
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
           :clear (let [updated-node (update node
                                            :set-projections
                                            into
                                            (map (fn [added] [added nil]))
                                            (get-in node [:state :vars]))]
                   (update-in updated-node [:outbox local-store-proc-dtor]
                     (fnil conj [])
                     {:commands [[storage [::store/assoc :curator-storage-form-example {}]]]})) ;; should dissoc not assoc?
           :submit (update-in node [:outbox local-store-proc-dtor]
                         (fnil conj [])
                         {:commands [[storage [::store/assoc :curator-storage-form-example (-> node :state :transient-input)]]]})))))})

(reg/def-registered-descriptor address-form-machine-spec' address-form-machine-spec address-form-machine-impl)

(def local-store-proc-impl
    {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/process
     ;; process manager/create
     ::pm/create (fn [pdtor] ;; process-descriptor
                   (reify pm/IProcess
                     (-send-message! [process {:keys [graph-value gm commands] :as effect}] ;; graph-value & gm inserted by `effect`
                       (nextTick (fn [] (hitch/apply-commands gm commands))))
                     (-kill-process! [process] true)))})
(reg/def-registered-descriptor local-store-proc-spec' local-store-proc-spec local-store-proc-impl)

;;;;;;; POS descriptor
(def-descriptor-spec form-field-spec
  :not-machine
  :canonical-form :map
  :positional-params [:curator-dtor :path])
(def form-field-impl
  {:hitch2.descriptor.impl/kind        :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator (fn [descriptor]
                                         (-> descriptor :term :curator-dtor))}) ;; ->dtor pronounced "create descriptor"
(reg/def-registered-descriptor form-field-spec' form-field-spec form-field-impl)
(defn form-field [curator-dtor path]
  (hitch/->dtor form-field-spec' {:curator-dtor curator-dtor :path path}))


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
      (d/div {} (str "Local Storage Value:  " field-stored-val))
      (d/div {} (str "Input Value:  " field-transient-val))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true} (RE Button {:onClick (fn [e]
                                                     (hitch/apply-commands graph
                                                       [[field-dtor [:value-change field-stored-val field-dtor]]]))}
                                "Discard"))
        (RE Grid {:item true} (RE Button {:onClick (fn [e]
                                                     (hitch/apply-commands graph
                                                       [[field-dtor [:submit-item field-transient-val field-dtor]]]))}
                                "Save"))))))

(defn formField [{:keys [graph label validation-fn form-line-dtor form-line-stored-dtor] :as props}]
  (let [
        form-line-val (hitch-hook/useSelected form-line-dtor)
        form-line-stored-val (hitch-hook/useSelected form-line-stored-dtor)
        formLineR (react/useRef)]
    (if (hitch-hook/loaded? form-line-val)
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true}
          (RE TextField {:label    label
                         :inputRef formLineR
                         :error    (if validation-fn (validation-fn form-line-val))
                         :onChange (fn [e]
                                     (hitch/apply-commands graph [[form-line-dtor [:value-change (.. e -target -value) form-line-dtor]]]))
                         :value    (or form-line-val "")})
          (when (.-current formLineR)
            (CE changes-popper {:field-transient-val form-line-val
                                :field-stored-val    form-line-stored-val
                                :field-dtor          form-line-dtor
                                :anchor-elem         formLineR
                                :graph               graph}))))
      (d/div {} "loading ..."))))

(defn curator-storage-form [{:keys [graph] :as props}]

  (let [form-dtor (hitch/->dtor address-form-machine-spec' {:keyspace :curator-storage-form-example})]
    (RE Paper {:style #js {"border"  "1px solid black"
                           "padding" "10px"}}
      (RE Paper {}
        (CE formField {:graph         graph
                       :form-line-dtor (form-field form-dtor [:transient :address-line])
                       :form-line-stored-dtor (form-field form-dtor [:localstorage :address-line])
                       :label         "Address"
                       :validation-fn (fn [form-field-val] (not (numeric? form-field-val)))})
        (CE formField {:graph        graph
                       :form-line-dtor (form-field form-dtor [:transient :city-line])
                       :form-line-stored-dtor (form-field form-dtor [:localstorage :city-line])
                       :label        "City"})
        (CE formField {:graph         graph
                       :form-line-dtor (form-field form-dtor [:transient :state-line])
                       :form-line-stored-dtor (form-field form-dtor [:localstorage :state-line])
                       :label         "State"
                       :validation-fn (fn [line-val] (false? (contains? (-> states vals set) line-val)))})
        (CE formField {:graph        graph
                       :form-line-dtor (form-field form-dtor [:transient :zip-line])
                       :form-line-stored-dtor (form-field form-dtor [:localstorage :zip-line])
                       :label        "Zip"}))
      (RE Grid {:container true :spacing 2}
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn [] (hitch/apply-commands graph [[form-dtor [:clear {}]]])))}
            "clear"))
        (RE Grid {:item true}
          (RE Button {:style   #js {:margin "10px"}
                      :onClick (react/useCallback
                                 (fn [] (hitch/apply-commands graph [[form-dtor [:submit {}]]])))}
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
