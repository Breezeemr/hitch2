(ns hitch2.protocols.selector
  #?(:cljs (:require-macros [hitch2.protocols.selector]))
  (:require [clojure.spec.alpha :as s])
  #?(:clj
     (:import (clojure.lang IPersistentMap IPersistentCollection ILookup Keyword
                            MapEntry APersistentVector IHashEq APersistentMap Indexed PersistentVector)
              (java.util Iterator)
              (java.io Writer))))

#?(:cljs nil
   :clj
   (defmacro def-selector-spec
     [selector-name kind & options]
     {:pre [(simple-symbol? selector-name)
            (keyword? kind)]}
     (let [sel-name (symbol
                      (some-> (or
                                (:name (:ns &env))          ;; targeting CLJS
                                (ns-name *ns*))             ;; targeting CLJ
                        name)
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
         (:hitch.selector.impl/halting impl))
       cljs.core/PersistentArrayMap
       (-get-halting-fn [impl]
         (:hitch.selector.impl/halting impl))]
      :clj
      ;; todo probably missing another map type
      [clojure.lang.PersistentArrayMap
       (-get-halting-fn [impl]
         (:hitch.selector.impl/halting impl))]))

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
       (nth [_ _ not-found] not-found)]
      :cljs
      [IIndexed
       (-nth [_ i] (case i
                     (throw (ex-info "Index out of bounds"
                                     {:selector :selector0
                                      :index    i}))))
       (-nth [_ i not-found] (case i
                               not-found))]))
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
                              not-found))]
      :cljs
      [IIndexed
       (-nth [_ i] (case i
                     0 a
                     (throw (ex-info "Index out of bounds"
                                        {:selector :selector1
                                         :index    i}))))
       (-nth [_ i not-found] (case i
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
                              not-found))]
      :cljs
      [IIndexed
       (-nth [_ i] (case i
                     0 a
                     1 b
                     (throw (ex-info "Index out of bounds"
                                     {:selector :selector0
                                      :index    i}))))
       (-nth [_ i not-found] (case i
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
                              not-found))]
      :cljs
      [IIndexed
       (-nth [_ i] (case i
                     0 a
                     1 b
                     2 c
                     (throw (ex-info "Index out of bounds"
                                     {:selector :selector0
                                      :index    i}))))
       (-nth [_ i not-found] (case i
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





;;; FRANCIS's Selector

(defprotocol IFastMapSubset
  "Struct or Record-like where some key lookups and vals are guaranteed to be faster."
  (overflow-map [o]
    "Get the map which has entries which are not fast-keys")
  (fast-keys [o]
    "Return a THING with only the fast keys.

    Keys will be distinct.")
  (fast-vals ^objects [o]
    "Return an array with only the fast values;

    Do not mutate the array."))

;; if slot-keys ends up being an array:
; (let [l (alength slot-keys)]
;         (loop [i 0]
;           (if (< i l)
;             (let [slot-k (aget slot-keys i)]
;               (if (identical? k slot-k)
;                 (aget slot-vals i)
;                 (recur (unchecked-inc-int i))))
;             (extmap k else))))

#?(:clj
   (defmacro ^:private fast-keyword-index
     [slotkeysym keysym]
     {:pre [(simple-symbol? slotkeysym) (simple-symbol? keysym)]}
     (with-meta
       `(let [~'it (.iterator ~slotkeysym)]
          (loop [~'i 0]
            (if (.hasNext ~'it)
              (let [~'sk (.next ~'it)]
                (if (identical? ~keysym ~'sk)
                  ~'i
                  (recur (unchecked-inc-int ~'i))))
              -1)))
       (assoc (meta &form) :tag 'int))))

#?(:clj
   (deftype SlottedSelectorIterator
     [^int ^:unsynchronized-mutable i
      ^int basecnt
      ^Iterator slot-key-iter
      ^objects slot-vals
      ^Iterator extmap-iter]
     Iterator
     (hasNext [_]
       (if (< i basecnt)
         true
         (.hasNext extmap-iter)))
     (next [_]
       (if (< i basecnt)
         (let [nextk (.next slot-key-iter)
               nextv (aget slot-vals i)]
           (set! i (unchecked-inc-int i))
           (MapEntry/create nextk nextv))
         (.next extmap-iter)))
     (remove [_] (UnsupportedOperationException.))))

;; HACK going to combine KeywordSlotMap with Selector for now
;; * selector-name and protocol should come off
;; * Indexed should move off

#?(:clj
   (deftype SlotedSelector
     [selector-name
      ^APersistentVector slot-keys
      ^objects slot-vals
      ^IPersistentMap extmap
      ^int ^:unsynchronized-mutable hash_
      ^int ^:unsynchronized-mutable hasheq_]

     java.io.Serializable
     IFastMapSubset
     (overflow-map [_] extmap)
     (fast-keys [_] slot-keys)
     (fast-vals [_] slot-vals)


     SelectorName
     (-sname [_] selector-name)

     Indexed
     (nth [_ i]
       (aget slot-vals i))
     (nth [_ i nf]
       (if (< i (alength slot-vals))
         (aget slot-vals i)
         nf))

     IHashEq
     (hasheq [this]
       (let [h hasheq_]
         (if (== 0 h)
           (let [h (clojure.lang.Util/hashCombine
                     (hash selector-name)
                     (APersistentMap/mapHasheq this))]
             (set! hasheq_ h)
             h)
           h)))

     Object
     (hashCode [this]
       (let [h hash_]
         (if (== 0 h)
           (let [h (APersistentMap/mapHash this)]
             (set! hasheq_ h)
             h)
           h)))
     (equals [this other] (APersistentMap/mapEquals this other))

     ILookup
     (valAt [this k]
       (.valAt this k nil))
     (valAt [_ k else]
       (let [^int i (fast-keyword-index slot-keys k)]
         (if (== i -1)
           (.valAt extmap k else)
           (aget slot-vals i))))

     IPersistentMap
     (count [_]
       (unchecked-add-int
         (alength slot-vals)
         (.count extmap)))
     (empty [_]
       (throw (UnsupportedOperationException. "Can't create empty KeywordSlotMap")))
     (cons [this e]
       (#'clojure.core/imap-cons this e))
     (equiv [this other]
       (or
         (identical? this other)
         (and
           (instance? SlotedSelector other)
           (= selector-name (-sname other))
           (.equiv ^IPersistentCollection slot-keys (fast-keys other))
           (let [other-vals (fast-vals other)]
             (or (identical? slot-vals other-vals)
               (let [ls (alength slot-vals) lo (alength other-vals)]
                 (and
                   (== ls lo)
                   (loop [i 0]
                     (if (< i ls)
                       (if (= (aget slot-vals i) (aget other-vals i))
                         (recur (unchecked-inc-int i))
                         false)
                       true))))))
           (.equiv extmap (overflow-map other)))))
     (containsKey [_ k]
       (let [^int i (fast-keyword-index slot-keys k)]
         (if (== -1 i)
           (.containsKey extmap k)
           true)))
     (entryAt [_ k]
       (let [^int i (fast-keyword-index slot-keys k)]
         (if (== -1 i)
           (.entryAt extmap k)
           (MapEntry/create k (aget slot-vals i)))))
     (seq [this]
       (iterator-seq (.iterator this)))
     (iterator [_]
       (assert (== (count slot-keys) (alength slot-vals))
         (pr-str selector-name slot-keys (seq slot-vals)))
       (->SlottedSelectorIterator
         0
         (alength slot-vals)
         (.iterator slot-keys)
         slot-vals
         (.iterator extmap)))
     (assoc [this k v]
       (let [^int i (fast-keyword-index slot-keys k)]
         (if (== -1 i)
           (let [extmap' (.assoc extmap k v)]
             (if (= extmap extmap')
               this
               (SlotedSelector. selector-name slot-keys slot-vals extmap' 0 0)))
           (if (= v (aget slot-vals i))
             this
             (let [slot-vals' (aclone slot-vals)]
               (aset slot-vals' i v)
               (SlotedSelector. selector-name slot-keys slot-vals' extmap 0 0))))))
     (without [this k]
       (let [^int i (fast-keyword-index slot-keys k)]
         (if (== -1 i)
           (let [extmap' (.without extmap k)]
             (if (identical? extmap extmap')
               this
               (SlotedSelector. selector-name slot-keys slot-vals extmap' 0 0)))
           (dissoc (into extmap
                     (map-indexed (fn [i k]
                                    (MapEntry/create k (aget slot-vals i))))
                     slot-keys) k))))

     java.util.Map
     (size [this] (.count this))
     (isEmpty [this] (== 0 (.count this)))
     (containsValue [this v] (boolean (some #(= v %) (vals this))))
     (get [this k] (.valAt this k))
     (put [this k v] (throw (UnsupportedOperationException.)))
     (remove [this k] (throw (UnsupportedOperationException.)))
     (putAll [this m] (throw (UnsupportedOperationException.)))
     (clear [this] (throw (UnsupportedOperationException.)))
     (keySet [this] (set (keys this)))
     (values [this] (vals this))
     (entrySet [this] (set this))))

#?(:clj
   (defmethod print-method SlotedSelector [ss ^Writer w]
     (.write w "#hitch.selector[")
     (print-method (-sname ss) w)
     (.write w " ")
     (print-method (zipmap (fast-keys ss) (fast-vals ss)) w)
     (.write w " ")
     (print-method (overflow-map ss) w)
     (.write w "]")))

(defn- raw-selector [name overflow slot-keys slot-vals]
  ;; INVARIANT: count slot-keys == alength slot-vals
  #?(:cljs (throw (ex-info "NOT IMPLEMENTED" {}))
     :clj  (->SlotedSelector name slot-keys slot-vals overflow 0 0)))

(defn selector? [x]
  #?(:cljs (throw (ex-info "NOT IMPLEMENTED" {}))
     :clj  (instance? SlotedSelector x)))

(defn- map->pos-sel [selname slot-keys data-map]
  (let [slot-vals (object-array (count slot-keys))
        extmap    (persistent!
                    (reduce-kv
                      (fn [data-map i k]
                        (let [v (data-map k)]
                          (aset slot-vals i v)
                          (dissoc! data-map k)))
                      (transient data-map)
                      slot-keys))]
    (raw-selector selname extmap slot-keys slot-vals)))

(let [empty-array (object-array 0)]
  (defn francis-sel [{selname :hitch.selector/name :as sspec} data]
    (if (selector? data)
      (if (= selname (-sname data))
        ;; fast path when we already have a similar selector
        (raw-selector selname (overflow-map data) (fast-keys data) (fast-vals data))
        (let [slot-keys (:hitch.selector.spec/positional-params sspec [])
              m         (into {} data)]
          (if (zero? (count slot-keys))
            (raw-selector selname m [] empty-array)
            (map->pos-sel selname slot-keys m))))
      (let [slot-keys (:hitch.selector.spec/positional-params sspec [])]
        (cond
          (zero? (count slot-keys))
          (raw-selector selname data [] empty-array)

          #?@(:clj
              ;; Zero-copy sharing of the underlying PV's tail array!
              [(and (instance? PersistentVector data)
                 ;; INVARIANT: if slot-keys >0, so should data be!
                 (<= (.count ^PersistentVector data) 32))
               (.arrayFor ^PersistentVector data 0)])

          (vector? data)
          ;;; TODO: in CLJ, when data is a PersistentVector, use arrayFor to avoid array copying
          (let [slot-vals (object-array (count slot-keys))]
            (reduce-kv (fn [_ i v] (aset slot-vals i v)) nil data)
            (raw-selector selname {} slot-keys slot-vals))

          ;; data is a map
          :else
          (map->pos-sel selname slot-keys data)))))


  (defn francis-psel
    ;; TODO: check for mismatch between params and args
    ([sspec]
     (raw-selector (:hitch.selector/name sspec) {}
       (:hitch.selector.spec/positional-params sspec [])
       empty-array))
    ([sspec a]
     (let [slot-vals (object-array 1)]
       (aset slot-vals 0 a)
       (raw-selector (:hitch.selector/name sspec) {}
         (:hitch.selector.spec/positional-params sspec [])
         slot-vals)))
    ([sspec a b]
     (let [slot-vals (object-array 2)]
       (aset slot-vals 0 a)
       (aset slot-vals 1 b)
       (raw-selector (:hitch.selector/name sspec) {}
         (:hitch.selector.spec/positional-params sspec [])
         slot-vals)))
    ([sspec a b c]
     (let [slot-vals (object-array 3)]
       (aset slot-vals 0 a)
       (aset slot-vals 1 b)
       (aset slot-vals 2 c)
       (raw-selector (:hitch.selector/name sspec) {}
         (:hitch.selector.spec/positional-params sspec [])
         slot-vals)))))
#_#_
(def sel tyler-sel)
(def map->sel tyler-map->sel)


(def sel francis-psel)
(def map->sel francis-sel)
