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


(defn mk-xhr [{:keys [url method serializer deserializer content headers withcreds]} cb]
  (let [xhr (XhrIo.)]
    (when withcreds
      (.setWithCredentials xhr true))
    (events/listen xhr EventType/SUCCESS
      (if deserializer
        (fn [e] (cb [:ok (deserializer (.. e -target (getResponseText)))]))
        (fn [e] (cb [:ok (.. e -target (getResponseText))]))))
    (events/listen xhr EventType/ERROR
      (fn [e] (cb [:error (.. e -target (getLastError))])))
    (.send xhr (str url) method (if serializer
                                  (serializer content)
                                  content) (clj->js headers))
    #(.dispose xhr)))

(def initial-node (assoc machine-proto/initial-node :state NOT-FOUND-SENTINEL))

(def http-impl (reify
                    sel-proto/ImplementationKind
                    (-imp-kind [machine] :hitch.selector.kind/machine)))

(def http-machine
  (reify
    sel-proto/SelectorImplementation
    (-imp [machine-instance] machine-impl)
    machine-proto/Init
    (-initialize [machine-instance] initial-node)
    machine-proto/ChildChanges
    (-child-changes [machine-instance graph-value node children parents children-added children-removed]
      node)
    machine-proto/Commandable
    (-apply-command [_ graph-value node children parents command]
      (case (nth command 0)
        :set-value (let [[_ val] command]
                     (-> node
                         (assoc :state val)
                         (update :reset-vars assoc (mutable-var ns) val)))
        :clear (-> node
                   (assoc :state NOT-FOUND-SENTINEL)
                   (update :reset-vars assoc (mutable-var ns) NOT-FOUND-SENTINEL))))))

(def var-impl
  (reify
    sel-proto/ImplementationKind
    (-imp-kind [var]
      :hitch.selector.kind/var)
    sel-proto/GetMachine
    (-get-machine [var sel]
      http-machine)))

(defn http [url method serializer deserializer content headers withcreds]
  (sel-proto/Selector1 var-impl
    {:url          url
     :method       method
     :serializer   serializer
     :deserializer deserializer
     :content      content
     :headers      headers
     :withcreds    withcreds}))
