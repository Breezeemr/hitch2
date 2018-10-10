(ns hitch2.curator.http
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto
             :refer-macros [def-selector-spec]]
            [goog.events :as events]
            [goog.net.EventType :as EventType]
            [clojure.string :as str]
            [hitch2.selector-impl-registry :as reg
             :refer-macros [def-registered-selector]]
            [hitch2.protocols.selector :as selector-proto])
  (:import (goog.net XhrIo)))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})


(defmethod graph-proto/run-effect ::request
  [gm {:keys [selector] :as effect}]
  (let [{:keys [url method serializer deserializer content headers withcreds]}
        selector
        cb  (fn [response]
              (graph-proto/-transact! gm selector [::value selector response]))
        xhr (XhrIo.)]
    (when withcreds
      (.setWithCredentials xhr true))
    (events/listen xhr EventType/SUCCESS
      (if deserializer
        (fn [e] (cb [:ok (deserializer (.. e -target (getResponseText)))]))
        (fn [e] (cb [:ok (.. e -target (getResponseText))]))))
    (events/listen xhr EventType/ERROR
      (fn [e] (cb [:error (.. e -target (getLastError))])))
    (.send xhr (str url) (meths method method)
           (if serializer
             (serializer content)
             content)
           (clj->js headers))
    #(.dispose xhr)))

(def initial-node machine-proto/initial-curator-state)
(def-selector-spec http-machine-spec
  :machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/positional)

(def http-machine-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    machine-proto/Init
    (-initialize [machine-instance machine-selector] initial-node)
    machine-proto/ChildChanges
    (-child-changes [machine-instance machine-selector graph-value node children-added children-removed]
      (update node :async-effects into (map (fn [child] {:type     ::request
                                                         :selector child})
                                         children-added)))
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node command]
      (case (nth command 0)
        ::value (let [[_ selector response] command]
                  (assoc-in node [:reset-vars selector] response))
        ::refresh (let [[_ selector] command]
                    (update node :async-effects conj {:type     ::request
                                                      :selector selector}))))))

(reg/def-registered-selector http-machine-spec' http-machine-spec http-machine-impl)
(def http-machine (sel-proto/sel http-machine-spec'))


(def-selector-spec http-spec
  :not-machine
  :hitch.selector.spec/canonical-form
  :hitch.selector.spec.canonical-form/map)

(def http-var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var)
    sel-proto/GetMachine
    (-get-machine [var sel]
      http-machine)))

(reg/def-registered-selector http-spec' http-spec http-var-impl)

(defn http [url method serializer deserializer content headers withcreds]
  (sel-proto/map->sel http-spec'
    {:url          url
     :method       method
     :serializer   serializer
     :deserializer deserializer
     :content      content
     :headers      headers
     :withcreds    withcreds}))
