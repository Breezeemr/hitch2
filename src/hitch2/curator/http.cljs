(ns hitch2.curator.http
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec
             :refer-macros [def-descriptor-spec]]
            [goog.events :as events]
            [goog.net.EventType :as EventType]
            [clojure.string :as str]
            [hitch2.descriptor :as descriptor]
            [hitch2.selector-impl-registry :as reg
             :refer-macros [def-registered-selector]])
  (:import (goog.net XhrIo)))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})


(defmethod graph-proto/run-effect ::request
  [gm {:keys [selector] :as effect}]
  (let [{:keys [url method serializer deserializer content headers withcreds]}
        (:term selector)
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
(def-descriptor-spec http-machine-spec
  :machine
  :hitch2.descriptor.spec/canonical-form
  :hitch2.descriptor.spec.canonical-form/positional)

(def http-machine-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/machine
   ::machine-proto/init (fn [machine-selector] initial-node)
   ::machine-proto/curation-changes
                             (fn [machine-selector graph-value node children-added children-removed]
                               (update node :async-effects into (map (fn [child] {:type     ::request
                                                                                  :selector child})
                                                                  children-added)))
   ::machine-proto/apply-command
                             (fn [machine-selector graph-value node command]
                               (case (nth command 0)
                                 ::value (let [[_ selector response] command]
                                           (assoc-in node [:set-projections selector] response))
                                 ::refresh (let [[_ selector] command]
                                             (assert selector (str (pr-str ::refresh) " must provide a selector"))
                                             (update node :async-effects conj {:type     ::request
                                                                               :selector selector}))))})

(reg/def-registered-selector http-machine-spec' http-machine-spec http-machine-impl)
(def http-machine (descriptor/dtor  http-machine-spec'))


(def-descriptor-spec http-spec
  :not-machine
  :hitch2.descriptor.spec/canonical-form
  :hitch2.descriptor.spec.canonical-form/map)

(def http-var-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-machine
   (fn [sel]
     http-machine)})

(reg/def-registered-selector http-spec' http-spec http-var-impl)

(defn http [url method serializer deserializer content headers withcreds]
  (descriptor/map->dtor  http-spec'
    {:url          url
     :method       method
     :serializer   serializer
     :deserializer deserializer
     :content      content
     :headers      headers
     :withcreds    withcreds}))
