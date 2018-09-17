(ns hitch2.protocols.selector
  #?(:cljs (:require-macros [hitch2.protocols.selector :refer [m->selector]])))

(comment
  we want to experiment with many different implimentations of selectors. The
  easiest way to do that is to have selector protocols. We will very likely
  settle on only one implimentation because we need it to always hash
  consistantly.

  When is invoke-halting called and for what purpose? It was mentioned that
  it is hot path but during what circumstances? Is this when something is not found?
  Or when it is found? Is this called while polling for the found data? Should there be
  an opaque function that someone constructs
  )

(defprotocol SelectorName
  (-sname [sel] "returns the selector name"))

(defprotocol InvokeHalting
  (-invoke-halting [_ f gv-tracker]))
(defprotocol InvokeHalting2
  (-invoke-halting2 [_ f gv-tracker]))
(defprotocol InvokeHalting3
  (-invoke-halting3 [_ f gv-tracker]))
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
       (-sname [sel] (:s-name sel))]))

(extend-protocol InvokeHalting
  #?@(:cljs
      [cljs.core/PersistentVector
       (-invoke-halting [sel f gv-tracker] (apply f (rest sel)))
       cljs.core/PersistentHashMap
       (-invoke-halting [sel f gv-tracker]
                        (f (dissoc sel :s-name)))
       cljs.core/PersistentArrayMap
       (-invoke-halting [sel f gv-tracker]
                        (f (dissoc sel :s-name)))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-invoke-halting [sel f gv-tracker]
                        (f (dissoc sel :s-name)))
       clojure.lang.PersistentVector
       (-invoke-halting [sel f gv-tracker] (apply f (rest sel)))]))
