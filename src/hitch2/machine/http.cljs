(ns hitch2.machine.http
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.machine :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.protocols.selector :as sel-proto]
            [goog.events :as events]
            [goog.net.EventType :as EventType]
            [clojure.string :as str])
  (:import (goog.net XhrIo)))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})


(defmethod graph-proto/run-effect ::request
  [gm {:keys [selector] :as effect}]
  (let [{:keys [url method serializer deserializer content headers withcreds]}
        (:a selector)
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

(def initial-node machine-proto/initial-machine-state)

(def http-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [machine] :hitch.selector.kind/machine)
    sel-proto/SelectorName
    (-sname [imp] "http machine")
    machine-proto/Init
    (-initialize [machine-instance machine-selector] initial-node)
    machine-proto/ChildChanges
    (-child-changes [machine-instance machine-selector graph-value node children parents children-added children-removed]
      (update node :async-effects into (map (fn [child] {:type     ::request
                                                         :selector child})
                                         children-added)))
    machine-proto/Commandable
    (-apply-command [_ machine-selector graph-value node children parents command]
      (case (nth command 0)
        ::value (let [[_ selector response] command]
                  (assoc-in node [:reset-vars selector] response))
        ::refresh (let [[_ selector] command]
                    (update node :async-effects conj {:type     ::request
                                                      :selector selector}))))))

(def http-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] http-impl)))

(def var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var)
    sel-proto/GetMachine
    (-get-machine [var sel]
      http-machine)))

(defn http [url method serializer deserializer content headers withcreds]
  (sel-proto/->Selector1 var-impl
    {:url          url
     :method       method
     :serializer   serializer
     :deserializer deserializer
     :content      content
     :headers      headers
     :withcreds    withcreds}))
