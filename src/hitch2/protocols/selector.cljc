(ns hitch2.protocols.selector
  (:require [clojure.spec.alpha :as s]))

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
(s/def :hitch.selector.impl/machine-finder fn?)
(s/def :hitch.selector.impl/halting fn?)
(s/def :hitch.selector.impl/sentinel fn?)
(s/def :hitch.selector.impl/dependent-value fn?)


(defmulti impliementation-kind :kind)

(defmethod impliementation-kind :hitch.selector.kind/var-singleton-machine
  [_]
  (s/keys
    :req [:hitch.selector/name]
    :req-un [:hitch.selector.impl/kind
             :hitch.selector.impl/machine]))

(defmethod impliementation-kind :hitch.selector.kind/var-machine
  [_]
  (s/keys
    :req [:hitch.selector/name]
    :req-un [:hitch.selector.impl/kind
             :hitch.selector.impl/machine-finder]))

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

(comment
  we want to experiment with many different implementations of selectors. The
  easiest way to do that is to have selector protocols. We will very likely
  settle on only one implementation because we need it to always hash
  consistently.

  When is invoke-halting called and for what purpose? It was mentioned that
  it is hot path but during what circumstances? Is this when something is not found?
  Or when it is found? Is this called while polling for the found data? Should there be
  an opaque function that someone constructs
  )

(defprotocol SelectorName
  (-sname [sel] "returns the selector name"))

(defprotocol InvokeHalting
  (-invoke-halting [_ f gv-tracker]))
;;f first?

(defrecord Selector0 [name]
  SelectorName
  (-sname [_] name)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker)))
(defrecord Selector1 [name a]
  SelectorName
  (-sname [_] name)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a)))
(defrecord Selector2 [name a b]
  SelectorName
  (-sname [_] name)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b)))

;;is gv-tracker the best name?


(extend-protocol SelectorName
  #?@(:cljs
      [cljs.core/PersistentVector
       (-sname [sel] (first sel))
       cljs.core/PersistentHashMap
       (-sname [sel] (:s-name sel))
       cljs.core/PersistentArrayMap
       (-sname [sel] (:s-name sel))]
      :clj
      [clojure.lang.PersistentVector
       (-sname [sel] (first sel))
       clojure.lang.PersistentArrayMap
       (-sname [sel] (:s-name sel))]))

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

(deftype TypeSelector1 [name a]
  InvokeHalting
  (-invoke-halting [sel f gv-tracker]
    (f gv-tracker a)))
