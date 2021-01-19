(ns hitch2.curator.http
  (:require [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.def.curator :as curator-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.def.spec
             :refer-macros [def-descriptor-spec]]
            [goog.events :as events]
            [goog.net.EventType :as EventType]
            [clojure.string :as str]
            [hitch2.descriptor :as descriptor]
            [hitch2.descriptor-impl-registry :as reg
             :refer-macros [def-registered-descriptor]])
  (:import (goog.net XhrIo)))

(def ^:private meths
  {:get    "GET"
   :put    "PUT"
   :post   "POST"
   :delete "DELETE"})


(defmethod graph-proto/run-effect ::request
  [gm {:keys [descriptor] :as effect}]
  (let [{:keys [url method serializer deserializer content headers withcreds]}
        (:term descriptor)
        cb  (fn [response]
              (graph-proto/-transact! gm descriptor [::value descriptor response]))
        xhr (XhrIo.)]
    (when withcreds
      (.setWithCredentials xhr true))
    (events/listen xhr EventType/SUCCESS
      (if deserializer
        (fn [^js e] (cb [:ok (deserializer (.. e -target (getResponseText)))]))
        (fn [^js e] (cb [:ok (.. e -target (getResponseText))]))))
    (events/listen xhr EventType/ERROR
      (fn [^js e] (cb [:error (.. e -target (getLastError))])))
    (.send xhr (str url) (meths method method)
           (if serializer
             (serializer content)
             content)
           (clj->js headers))
    #(.dispose xhr)))

(def initial-node curator-proto/initial-curator-state)
(def-descriptor-spec http-curator-spec
  :curator)

(def http-curator-impl
  {:hitch2.descriptor.impl/kind
   :hitch2.descriptor.kind/curator
   ::curator-proto/init
   (fn [curator-descriptor] initial-node)
   ::curator-proto/curation-changes
   (fn [curator-descriptor]
     (fn [graph-value node children-added children-removed]
       (update node :async-effects into (map (fn [child] {:type       ::request
                                                          :descriptor child})
                                             children-added))))
   ::curator-proto/apply-command
   (fn [curator-descriptor]
     (fn [graph-value node command]
       (case (nth command 0)
         ::value (let [[_ descriptor response] command]
                   (assoc-in node [:set-projections descriptor] response))
         ::refresh (let [[_ descriptor] command]
                     (assert descriptor (str (pr-str ::refresh) " must provide a descriptor"))
                     (update node :async-effects conj {:type       ::request
                                                       :descriptor descriptor})))))})

(reg/def-registered-descriptor http-curator-spec' http-curator-spec http-curator-impl)
(def http-curator (descriptor/->dtor  http-curator-spec' nil))


(def-descriptor-spec http-spec
  :not-curator
  :canonical-form
  :map
  :positional-params [:url
                      :method
                      :content
                      :headers
                      :withcreds
                      :serializer
                      :deserializer
                      ])

(def http-var-impl
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator
   (fn [sel]
     http-curator)})

(reg/def-registered-descriptor http-spec' http-spec http-var-impl)

(defn http [url method serializer deserializer content headers withcreds]
  (descriptor/->dtor  http-spec'
    {:url          url
     :method       method
     :serializer   serializer
     :deserializer deserializer
     :content      content
     :headers      headers
     :withcreds    withcreds}))
