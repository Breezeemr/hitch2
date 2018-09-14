(ns hitch2.protocols.selector)

(comment
  we want to experiment with many different implimentations of selectors. The
  easiest way to do that is to have selector protocols. We will very likely
  settle on only one implimentation because we need it to always hash
  consistantly.
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

;is gv-tracker the best name?
