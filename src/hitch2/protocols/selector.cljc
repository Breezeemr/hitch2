(ns hitch2.protocols.selector
  #?(:cljs (:require-macros [hitch2.protocols.selector]))
  (:require [clojure.spec.alpha :as s]))

#?(:cljs nil
   :clj
   (defmacro def-selector-spec
     [selector-name kind & options]
     {:pre [(simple-symbol? selector-name)
            (keyword? kind)]}
     (let [sel-name (symbol
                      (or
                        (str (:name (:ns &env)))   ;; targeting CLJS
                        (name (ns-name *ns*)))              ;; targeting CLJ
                      (name selector-name))
           qkind    (if (qualified-keyword? kind)
                      kind
                      (keyword "hitch.selector.spec.kind" (name kind)))]
       `(def ~selector-name
          (array-map
            ~@options
            :hitch.selector/name ~(list 'quote sel-name)
            :hitch.selector.spec/kind ~qkind)))))

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

(s/def :hitch.selector/name qualified-symbol?)
(s/def :hitch.selector.spec/kind
  #{:hitch.selector.spec.kind/machine
    :hitch.selector.spec.kind/not-machine})


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
    (f gv-tracker))
  #?@(:clj
      [clojure.lang.Indexed
       (nth [_ _] (throw (IndexOutOfBoundsException. "Selector0 has no arguments.")))
       (nth [_ _ not-found] not-found)]))
(defrecord Selector1 [impl a]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a))
  #?@(:clj 
      [clojure.lang.Indexed
       (nth [_ i] (case i
                    0 a
                    (throw (IndexOutOfBoundsException. "Selector0 has no arguments."))))
       (nth [_ i not-found] (case i
                              0 a
                              not-found))]))
(defrecord Selector2 [impl a b]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b))
  #?@(:clj
      [clojure.lang.Indexed
       (nth [_ i] (case i
                    0 a
                    1 b
                    (throw (IndexOutOfBoundsException. "Selector0 has no arguments."))))
       (nth [_ i not-found] (case i
                              0 a
                              1 b
                              not-found))]))

(defrecord Selector3 [impl a b c]
  SelectorName
  (-sname [_] impl)
  InvokeHalting
  (-invoke-halting [_ f gv-tracker]
    (f gv-tracker a b c))
  #?@(:clj
      [clojure.lang.Indexed
       (nth [_ i] (case i
                    0 a
                    1 b
                    2 c
                    (throw (IndexOutOfBoundsException. "Selector0 has no arguments."))))
       (nth [_ i not-found] (case i
                              0 a
                              1 b
                              2 c
                              not-found))]))


;;is gv-tracker the best name?


(extend-protocol SelectorName
  #?@(:cljs
      [cljs.core/PersistentVector
       (-sname [sel] (first sel))
       cljs.core/PersistentHashMap
       (-sname [sel] (:hitch.selector/name sel))
       cljs.core/PersistentArrayMap
       (-sname [sel] (:hitch.selector/name sel))]
      :clj
      [clojure.lang.PersistentVector
       (-sname [sel] (first sel))
       clojure.lang.APersistentMap
       (-sname [sel] (:hitch.selector/name sel))]))

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

(defn tyler-sel
  ([selector-spec]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Selector0 (:hitch.selector/name selector-spec))
     :hitch.selector.spec.canonical-form/map
     {:hitch.selector/name (:hitch.selector/name selector-spec)}))
  ([selector-spec a]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Selector1 (:hitch.selector/name selector-spec) a)
     :hitch.selector.spec.canonical-form/map
     (let [params  (:hitch.selector.spec/positional-params selector-spec)
           _       (assert (= 1 (count params)))
           [a-key] params]
       {:hitch.selector/name (:hitch.selector/name selector-spec)
        a-key                a}))
    )
  ([selector-spec a b]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Selector2 (:hitch.selector/name selector-spec) a b)
     :hitch.selector.spec.canonical-form/map
     (let [params        (:hitch.selector.spec/positional-params selector-spec)
           _             (assert (= 2 (count params)))
           [a-key b-key] params]
       {:hitch.selector/name (:hitch.selector/name selector-spec)
        a-key                a
        b-key                b}))
    )
  ([selector-spec a b c]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Selector3 (:hitch.selector/name selector-spec)  a b c)
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params selector-spec)
           _                   (assert (= 3 (count params)))
           [a-key b-key c-key] params]
       {:hitch.selector/name (:hitch.selector/name selector-spec)
        a-key                a
        b-key                b
        c-key                c}))))

(def sel tyler-sel)

(defn tyler-map->sel [selector-spec data]
  (case (:hitch.selector.spec/canonical-form selector-spec)
    :hitch.selector.spec.canonical-form/positional
    (let [positional-params (:hitch.selector.spec/positional-params selector-spec)]
      (case (count positional-params)
        0 (->Selector0 (:hitch.selector/name selector-spec))
        1 (let [[a] positional-params]
            (->Selector1 (:hitch.selector/name selector-spec) (a data)))
        2 (let [[a b] positional-params]
            (->Selector2 (:hitch.selector/name selector-spec) (a data) (b data)))
        3 (let [[a b c] positional-params]
            (->Selector3 (:hitch.selector/name selector-spec) (a data) (b data) (c data)))))
    :hitch.selector.spec.canonical-form/map
    (assoc data :hitch.selector/name (:hitch.selector/name selector-spec))))

(def map->sel tyler-map->sel)
