(ns hitch2.protocols.selector
  (:require [clojure.spec.alpha :as s]))


;; selector kind? what generalizes selector and machine? Graphable?
;; Hitchable? HitchKind Hitchtype?
(defprotocol ImplementationKind
  (-imp-kind [impl]
    "Returns the kind of selector or machine.
Should be a keyword for dispatching. Values are from:
:hitch.selector.kind/var
:hitch.selector.kind/machine
:hitch.selector.kind/sentinel
:hitch.selector.kind/halting"))

(extend-protocol ImplementationKind
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-imp-kind [impl]
         (:kind impl))
       cljs.core/PersistentArrayMap
       (-imp-kind [impl]
         (:kind impl))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-imp-kind [impl]
         (:kind impl))]))

(defprotocol HaltingImplementation
  (-get-halting-fn [imp]))

(extend-protocol HaltingImplementation
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-get-halting-fn [impl]
         (:halting impl))
       cljs.core/PersistentArrayMap
       (-get-halting-fn [impl]
         (:halting impl))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-get-halting-fn [impl]
         (:halting impl))]))

(defprotocol SentinelImplementation
  (-get-sentinel-fn [imp]))

(defprotocol SelectorName
  (-sname [imp] "returns the selector name"))

(defprotocol GetMachine
  (-get-machine [impl sel]
    "return a machine selector from impl"))

(s/def :hitch.selector.spec/specs any?)
(s/def :hitch.selector.spec/name keyword?)
(s/def :hitch.selector.spec/args :hitch.selector.spec/specs)
(s/def :hitch.selector.spec/value :hitch.selector.spec/specs)
(s/def :hitch.selector.spec/params (s/coll-of keyword?))

(s/def :selector/spec (s/keys
                        :req [:hitch.selector/name]
                        :req-un [:hitch.selector.spec/args
                                 :hitch.selector.spec/value
                                 :hitch.selector.spec/params]))

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
  (s/multi-spec impliementation-kind :kind))

(defn selector-name [sel]
  (-sname sel))

(defn has-name? [selector]
  (selector-name selector))

(s/def :selector/selector has-name?)



(defprotocol InvokeHalting
  (-invoke-halting [_ f gv-tracker]))
;;f first?

(defrecord Selector0 [impl]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker)))
(defrecord Selector1 [impl a]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a)))
(defrecord Selector2 [impl a b]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b)))

(defrecord Selector3 [impl a b c]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b c)))


;;is gv-tracker the best name?


(extend-protocol SelectorName
  #?@(:cljs
      [cljs.core/PersistentVector
       (-sname [sel] (first sel))
       cljs.core/PersistentHashMap
       (-sname [sel] (:selector-name sel))
       cljs.core/PersistentArrayMap
       (-sname [sel] (:selector-name sel))]
      :clj
      [clojure.lang.PersistentVector
       (-sname [sel] (first sel))
       clojure.lang.PersistentArrayMap
       (-sname [sel] (:selector-name sel))]))

;; todo: what does a vector do?
;; cljs.core/PersistentVector
;; clojure.lang.PersistentVector
(extend-protocol ImplementationKind
  #?@(:cljs
      [cljs.core/PersistentHashMap
       (-imp-kind [sel] (:kind sel))
       cljs.core/PersistentArrayMap
       (-imp-kind [sel] (:kind sel))]
      :clj
      [clojure.lang.PersistentArrayMap
       (-imp-kind [sel] (:kind sel))]))

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

(deftype TypeSelector1 [impl a]
  InvokeHalting
  (-invoke-halting [sel f gv-tracker]
    (f gv-tracker a)))
