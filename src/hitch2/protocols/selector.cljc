 (ns hitch2.protocols.selector
  #?(:cljs (:require-macros [hitch2.protocols.selector]))
  (:require [clojure.spec.alpha :as s])
  #?(:clj
     (:import (clojure.lang IPersistentMap IPersistentCollection ILookup Keyword
                            MapEntry APersistentVector IHashEq APersistentMap Indexed PersistentVector)
              (java.util Iterator)
              (java.io Writer))))

;; selector kind? what generalizes selector and curator? Graphable?
;; Hitchable? HitchKind Hitchtype?
(defprotocol ImplementationKind
  (-imp-kind [impl]
    "Returns the kind of selector or curator.
Should be a keyword for dispatching. Values are from:
:hitch.selector.kind/var
:hitch.selector.kind/curator
:hitch.selector.kind/sentinel
:hitch.selector.kind/halting"))

(extend-protocol ImplementationKind
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-imp-kind [impl]
         (:hitch.selector.impl/kind impl))
       cljs.core/PersistentArrayMap
       (-imp-kind [impl]
         (:hitch.selector.impl/kind impl))]
      :clj
      [clojure.lang.APersistentMap
       (-imp-kind [impl]
         (:hitch.selector.impl/kind impl))]))

(defprotocol HaltingImplementation
  (-get-halting-fn [imp]))

(extend-protocol HaltingImplementation
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-get-halting-fn [impl]
         (:hitch.selector.impl/halting impl))
       cljs.core/PersistentArrayMap
       (-get-halting-fn [impl]
         (:hitch.selector.impl/halting impl))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-get-halting-fn [impl]
         (:hitch.selector.impl/halting impl))]))

(defn get-machine [impl sel]
  (if-some [f (:hitch.selector.impl/get-machine impl)]
    (f sel)
    (assert false)))

(s/def :hitch.selector/name qualified-symbol?)
(s/def :hitch.selector.spec/kind
  #{:hitch2.descriptor.spec.kind/machine
    :hitch2.descriptor.spec.kind/not-machine})


(s/def :hitch.selector.spec/canonical-form
  #{:hitch.selector.spec.canonical-form/positional
    :hitch.selector.spec.canonical-form/map})

(s/def :hitch.selector.spec/positional-params
  (s/coll-of keyword? :kind vector? :into []))

(s/def :hitch/selector-spec
  (s/keys
    :req [:hitch.selector/name
          :hitch.selector.spec/kind]))

(s/def :hitch.selector.impl/kind keyword?)

(s/def :hitch.selector.impl/machine any?)
(s/def :hitch.selector.impl/halting fn?)
(s/def :hitch.selector.impl/sentinel fn?)
(s/def :hitch.selector.impl/dependent-value fn?)


(defmulti impliementation-kind -imp-kind)

(defmethod impliementation-kind :hitch.selector.kind/var
  [_]
  (s/keys
    :req [:hitch.selector/name]
    :req-un [:hitch.selector.impl/kind
             :hitch.selector.impl/machine]))

(defmethod impliementation-kind :hitch.selector.kind/halting
  [_]
  (s/keys
    :req [:hitch.selector/name]
    :req-un [:hitch.selector.impl/kind
             :hitch.selector.impl/halting]))
(defmethod impliementation-kind :hitch.selector.kind/sentinel
  [_]
  (s/keys
    :req [:hitch.selector/name]
    :req-un [:hitch.selector.impl/kind
             :hitch.selector.impl/sentinel]))

(s/def :selector/impl
  (s/multi-spec impliementation-kind :hitch.selector.impl/kind))

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
