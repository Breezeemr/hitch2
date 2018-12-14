 (ns hitch2.protocols.selector
  #?(:cljs (:require-macros [hitch2.protocols.selector]))
  (:require [clojure.spec.alpha :as s])
  #?(:clj
     (:import (clojure.lang IPersistentMap IPersistentCollection ILookup Keyword
                            MapEntry APersistentVector IHashEq APersistentMap Indexed PersistentVector)
              (java.util Iterator)
              (java.io Writer))))

(defn get-machine [impl sel]
  (if-some [f (:hitch.selector.impl/get-machine impl)]
    (f sel)
    (assert false)))

(s/def :hitch2.descriptor/name qualified-symbol?)
(s/def :hitch2.descriptor.spec/kind
  #{:hitch2.descriptor.spec.kind/machine
    :hitch2.descriptor.spec.kind/not-machine})


(s/def :hitch.selector.spec/canonical-form
  #{:hitch.selector.spec.canonical-form/positional
    :hitch.selector.spec.canonical-form/map})

(s/def :hitch.selector.spec/positional-params
  (s/coll-of keyword? :kind vector? :into []))

(s/def :hitch/selector-spec
  (s/keys
    :req [:hitch2.descriptor/name
          :hitch2.descriptor.spec/kind]))

(s/def :hitch2.descriptor.impl/kind keyword?)

(s/def :hitch.selector.impl/machine any?)
(s/def :hitch.selector.impl/halting fn?)
(s/def :hitch.selector.impl/sentinel fn?)
(s/def :hitch.selector.impl/dependent-value fn?)


(defmulti impliementation-kind :hitch2.descriptor.impl/kind)

(defmethod impliementation-kind :hitch.selector.kind/var
  [_]
  (s/keys
    :req [:hitch2.descriptor/name]
    :req-un [:hitch2.descriptor.impl/kind
             :hitch.selector.impl/machine]))

(defmethod impliementation-kind :hitch.selector.kind/halting
  [_]
  (s/keys
    :req [:hitch2.descriptor/name]
    :req-un [:hitch2.descriptor.impl/kind
             :hitch.selector.impl/halting]))
(defmethod impliementation-kind :hitch.selector.kind/sentinel
  [_]
  (s/keys
    :req [:hitch2.descriptor/name]
    :req-un [:hitch2.descriptor.impl/kind
             :hitch.selector.impl/sentinel]))

(s/def :selector/impl
  (s/multi-spec impliementation-kind :hitch2.descriptor.impl/kind))

(defn has-name? [selector]
  (:name selector))

(s/def :selector/selector has-name?)



(defprotocol InvokeHalting
  (-invoke-halting [_ f gv-tracker]))
;;f first?

;;is gv-tracker the best name?
(extend-protocol InvokeHalting
  #?@(:cljs
      [cljs.core/PersistentVector
       (-invoke-halting [sel f gv-tracker]
         (case (count sel)
           1 (f gv-tracker)
           2 (let [[a] sel] (f gv-tracker a))
           3 (let [[a b] sel] (f gv-tracker a b))))
       cljs.core/PersistentHashMap
       (-invoke-halting [sel f gv-tracker]
                        (f gv-tracker sel))
       cljs.core/PersistentArrayMap
       (-invoke-halting [sel f gv-tracker]
                        (f gv-tracker sel))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-invoke-halting [sel f gv-tracker]
                        (f gv-tracker sel))
       clojure.lang.PersistentVector
       (-invoke-halting [sel f gv-tracker]
         (case (count sel)
           1 (f gv-tracker)
           2 (let [[a] sel] (f gv-tracker a))
           3 (let [[a b] sel] (f gv-tracker a b))))]))
