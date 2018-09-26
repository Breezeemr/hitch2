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

(defprotocol HaltingImplementation
  (-get-halting-fn [imp]))

(defprotocol SentinelImplementation
  (-get-sentinel-fn [imp]))

(defprotocol SelectorImplementation
  (-imp [sel]
    "Returns the selector implementation"))

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

(defn selector-kind [sel]
  (some-> sel -imp -imp-kind))

(defn selector-name [sel]
  (some-> sel -imp -sname))

(defn has-impl? [selector]
  (and (satisfies? SelectorImplementation selector)
    (-imp selector)))

(defn has-name? [selector]
  (selector-name selector))

(defn has-kind? [selector]
  (selector-kind selector))

(s/def :selector/selector (s/and
                            has-impl?
                            (s/or
                              :full-impl has-kind?
                              :name has-name?)))



(defprotocol InvokeHalting
  (-invoke-halting [_ f gv-tracker]))
;;f first?

(defrecord Selector0 [impl]
  SelectorImplementation
  (-imp [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker)))
(defrecord Selector1 [impl a]
  SelectorImplementation
  (-imp [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a)))
(defrecord Selector2 [impl a b]
  SelectorImplementation
  (-imp [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b)))

(defrecord Selector3 [impl a b c]
  SelectorImplementation
  (-imp [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b c)))


;;is gv-tracker the best name?


(extend-protocol SelectorImplementation
  #?@(:cljs
      [cljs.core/PersistentVector
       (-imp [sel] (first sel))
       cljs.core/PersistentHashMap
       (-imp [sel] (:impl sel))
       cljs.core/PersistentArrayMap
       (-imp [sel] (:impl sel))]
      :clj
      [clojure.lang.PersistentVector
       (-imp [sel] (first sel))
       clojure.lang.PersistentArrayMap
       (-imp [sel] (:impl sel))]))

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
