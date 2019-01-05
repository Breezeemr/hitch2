(ns hitch2.descriptor-maps.robin-hood
  "A mutable descriptor hash map and hash set implemented with robin hood
  hashing. See:
  http://codecapsule.com/2013/11/11/robin-hood-hashing/
  http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
  https://probablydance.com/2017/02/26/i-wrote-the-fastest-hashtable/

  Keys must be Descriptor defrecords, or at least cljs persistent objects
  implementing IHash directly.
  Anything else may result in undefined behavior, if it works at all!

  This variant uses backward-shift deletion, a log2(n-buckets) probe limit,
  and a load factor of 0.95.
  It grows automatically with a probe length threshold or when probe limit is
  exceeded but only shrinks when asked.")

(def LOAD-FACTOR 0.95)

(defn- do-throw [s]
  (throw (js/Error.)))

(deftype DescriptorMapIter
  [^array arr ^:mutable ^number idx ^:mutable ^number unseen]
  Object
  (hasNext [_]
    (pos? unseen))
  (next [_]
    (loop [i idx]
      (if-some [e (aget arr i)]
        (do
          (set! unseen (dec unseen))
          (set! idx (inc i))
          e)
        (recur (inc i))))))

(defn- next-pow-2 [^number n]
  (let [n (bit-or n (unsigned-bit-shift-right n 1))
        n (bit-or n (unsigned-bit-shift-right n 2))
        n (bit-or n (unsigned-bit-shift-right n 4))
        n (bit-or n (unsigned-bit-shift-right n 8))
        n (bit-or n (unsigned-bit-shift-right n 16))]
    (bit-or (inc n) 0)))

(defn- hash-shift-for-size ^number [^number size]
  ;; INVARIANT: size must be an exact power of 2
  (bit-or (- 32 ^number (bit-or (.log2 js/Math (bit-or size 0)) 0)) 0))

;; TODO: suspect this is buggy, but not sure
#_(defn- transfer!
    "Copy all entries from old array to new array. Used for hash table resizing."
    [^number cnt ^array oarr ^array narr ^number nhash-shifts]
    (let [l (alength oarr)]
      (loop [i 0 seen 0]
        (when (and (< i l) (< seen cnt))
          (let [me (aget oarr i)]
            (if (undefined? me)
              (recur (inc i) seen)
              (let [oni (unsigned-bit-shift-right
                          (-hash ^not-native (-key ^not-native me))
                          nhash-shifts)]
                (loop [^number j oni ^number edib 0 ^not-native me me]
                  (let [fe (aget narr j)]
                    (if (undefined? fe)
                      (aset narr j me)
                      ;; collision in new array
                      (let [fni  (unsigned-bit-shift-right
                                   (-hash ^not-native fe)
                                   nhash-shifts)
                            fdib (- j fni)]
                        (if (> edib fdib)
                          ;; if DIB to insert is > DIB in entry, swap entries and keep probing
                          (do
                            (aset narr j me)
                            (recur (inc j) (inc fdib) fe))
                          ;; otherwise keep probing with the same entry
                          (recur (inc j) (inc edib) me))))))
                (recur (inc i) (inc seen))))))))
    js/undefined)

(defn- descriptor-map-equiv?* ^boolean [^not-native dm1 ^not-native coll]
  ;; INVARIANT: dm1 must be a DescriptorMap with descriptor keys
  ;; INVARIANT: dm1 and coll have the same item count
  ;; INVARIANT: coll is a cljs type supporting find
  ;; dm1's backing array should be smaller than coll's. Why?
  ;; dm1 is iterated, so you want the smallest array (fewest empty buckets)
  ;; coll is looked up in, so you want the fewest collisions (less probing)
  (let [it (-iterator dm1)]
    (loop []
      (if ^boolean (.hasNext it)
        (let [^not-native x (.next it)
              k             (-key x)
              y             (find coll k)]
          (if (-equiv x y)
            (recur)
            false))
        true))))

(declare DescriptorMap)
(defn descriptor-map-equiv?
  "Return true if dm1 and dm2 (both map-like and descriptor-keyed) have the
  same entries.
  Different from -equiv (cljs.core/=) on descriptor maps because they only
  compare equal to other descriptor maps. This function allows comparing
  DescriptorMaps with any lookup-able thing."
  ^boolean [dm1 dm2]
  (cond
    (and
      (instance? DescriptorMap dm1)
      (instance? DescriptorMap dm2))
    (if (== (-count ^not-native dm1) (-count ^not-native dm2))
      (let [dm1s (alength (.-arr dm1))
            dm2s (alength (.-arr dm2))]
        (if (> dm1s dm2s)
          (descriptor-map-equiv?* dm2 dm1)
          (descriptor-map-equiv?* dm1 dm2)))
      false)

    (instance? DescriptorMap dm1)
    (if (== (-count ^not-native dm1) (count dm2))
      ;; NOTE: Assumes we can iterate faster than any other persistent collection
      ;; (May not be true for very sparse DM that needs shrinking)
      (descriptor-map-equiv?* dm1 dm2)
      false)

    (instance? DescriptorMap dm2)
    (if (== (count dm1) (-count ^not-native dm2))
      (descriptor-map-equiv?* dm2 dm1)
      false)

    :else
    (do-throw "At least one argument to descriptor-map-equiv? must be a DescriptorMap")))

(defn- descriptor-set-equiv?* ^boolean [^not-native dm1 ^not-native coll]
  ;; INVARIANT: dm1 must be the backing DescriptorMap of a DescriptorSet
  ;; INVARIANT: dm1 and coll have the same item count
  ;; INVARIANT: coll is a cljs type supporting contains?
  ;; dm1's backing array should be smaller than coll's.
  (let [it (-iterator dm1)]
    (loop []
      (if ^boolean (.hasNext it)
        (let [^not-native x (.next it)
              k             (-key x)]
          (if (contains? coll k)
            (recur)
            false))
        true))))

(declare DescriptorSet)
(defn descriptor-set-equiv?
  "Return true if ds1 and ds2 (both set-like and containing only descriptors)
  have the same entries.
  Different from -equiv (cljs.core/=) on descriptor sets because they only
  compare equal to other descriptor set. This function allows comparing
  DescriptorSets with any contains?-able thing"
  ^boolean [ds1 ds2]
  (cond
    (and
      (instance? DescriptorSet ds1)
      (instance? DescriptorSet ds2))
    (if (== (-count ^not-native ds1) (-count ^not-native ds2))
      (let [dm1  (.-dm ds1)
            dm2  (.-dm ds2)
            dm1s (alength (.-arr dm1))
            dm2s (alength (.-arr dm2))]
        (if (> dm1s dm2s)
          (descriptor-set-equiv?* dm2 ds1)
          (descriptor-set-equiv?* dm1 ds2)))
      false)

    (instance? DescriptorSet ds1)
    (if (== (-count ^not-native ds1) (count ds2))
      (descriptor-set-equiv?* (.-dm ds1) ds2)
      false)

    (instance? DescriptorSet ds2)
    (if (== (count ds1) (-count ^not-native ds2))
      (descriptor-set-equiv?* (.-dm ds2) ds1)
      false)

    :else
    (do-throw "At least one argument to descriptor-set-equiv? must be a DescriptorSet")))

(defn- rconj! [dm entry]
  (cond
    (map-entry? entry)
    (.insert! dm entry)
    (vector? entry)
    (.insert! dm (->MapEntry (-nth ^not-native entry 0) (-nth ^not-native entry 1) nil))
    true
    (do-throw "conj! on a descriptor-map takes map entries or reducible of map entries"))
  dm)

;;; hash-shifts is the number of bits to shift (>>>) a 32-bit hash to get a
;;; bucket index. This is always (32 - Math.log2(arr.count))
(deftype DescriptorMap
  [^:mutable ^number cnt ^:mutable ^number hash-shifts ^:mutable ^array arr]
  Object
  (grow! [o]
    (let [oarr         arr
          ocnt         cnt
          nhash-shifts (dec hash-shifts)
          narr         (make-array (next-pow-2 (alength arr)))]
      (set! hash-shifts nhash-shifts)
      (set! arr narr)
      (set! cnt 0)
      (let [l (alength oarr)]
        (loop [i 0]
          (when (and (< i l) (< cnt ocnt))
            (let [me (aget oarr i)]
              (if (undefined? me)
                (recur (inc i))
                ;; TODO: could possibly do better than insert! here because of
                ;; some invariants during copying (see transfer!)
                (do (.insert! o me)
                    (recur (inc i))))))))
      (set! (.-length oarr) 0))
    js/undefined)

  (shrink! [o]
    (let [ocnt     cnt
          new-size (next-pow-2 ocnt)]
      (when-not (= new-size (alength arr))
        (let [oarr         arr
              nhash-shifts (hash-shift-for-size new-size)
              narr         (make-array new-size)]
          (set! hash-shifts nhash-shifts)
          (set! arr narr)
          (set! cnt 0)
          (let [l (alength oarr)]
            (loop [i 0]
              (when (and (< i l) (< cnt ocnt))
                (let [me (aget oarr i)]
                  (if (undefined? me)
                    (recur (inc i))
                    ;; TODO: could possibly do better than insert! here because of
                    ;; some invariants during copying (see transfer!)
                    (do (.insert! o me)
                        (recur (inc i))))))))
          (set! (.-length oarr) 0))))
    js/undefined)

  (maybe-grow! [o]
    (when (> (/ cnt (alength arr)) LOAD-FACTOR)
      (.grow! o))
    js/undefined)

  (insert! [o ^not-native me]
    (let [oni (unsigned-bit-shift-right (-hash ^not-native (-key me)) hash-shifts)]
      (loop [^number i oni ^number ni oni ^not-native me me]
        (let [fe (aget arr i)]                              ; fe = found entry
          (if (undefined? fe)
            ;; Bucket is empty, insert the entry
            ;; This check also handles overflow (i.e. if we probe past the end of
            ;; the arr) because in javascript (aset arr (alength arr) x) is the
            ;; same as (.push arr x), and i will always be <= (alength arr)
            (do (set! cnt (inc cnt))
                (aset arr i me)
                (.maybe-grow! o))
            (let [^not-native fk (-key ^not-native fe)
                  fh             (-hash fk)]
              (if (and (== (-hash ^not-native (-key me)) fh) (-equiv (-key me) fk))
                (do
                  (aset arr i (->MapEntry fk (-val me) (.-__hash me))))
                ;; collision; measure DIB (distance to initial bucket) to see
                ;; if we should swap or keep probing
                (let [fni  (unsigned-bit-shift-right fh hash-shifts)
                      fdib (- i fni)
                      edib (- i ni)]
                  (if (< fdib edib)
                    ;; if DIB of what we want to insert is > DIB in entry,
                    ;; swap entries and keep probing
                    (do
                      (aset arr i me)
                      (recur (inc i) fni fe))
                    ;; check if we hit the probe distance limit
                    ;; probe limit is log2(arr-size), which is the same as
                    ;; 32 - hash-shifts.
                    (do
                      (if (> (- (inc i) oni) (- 32 hash-shifts))
                        ;; our distance from the initial natural index exceeds
                        ;; the max probe length. We're colliding too badly,
                        ;; time to resize
                        (do (.grow! o)
                            (.insert! o me))
                        ;; otherwise keep probing with the same entry
                        (recur (inc i) ni me)))
                    ))))))))
    js/undefined)

  (remove! [o ^not-native k]
    (let [h      (-hash k)
          ni     (unsigned-bit-shift-right h hash-shifts)
          ;; find the actual bucket index this key is in
          starti (loop [ki ni]
                   (let [e (aget arr ki)]
                     (if (undefined? e)
                       -1
                       (let [^not-native fk (-key ^not-native e)
                             fh             (-hash fk)]
                         (if (and (== h fh) (-equiv k fk))
                           ki
                           (if (< ki (alength arr))
                             ;; See comment on -find that explains this optimization
                             (let [fni  (unsigned-bit-shift-right fh hash-shifts)
                                   fdib (- ki fni)
                                   edib (- ki ni)]
                               #_(assert (>= fdib 0))
                               #_(assert (>= edib 0))
                               (if (< fdib edib)
                                 -1
                                 (recur (inc ki))))
                             -1))))))]
      ;; now continue, this time shifting items back one until we reach a
      ;; an entry in its natural place (DIB=0) or empty or fall off the array
      (when-not (== -1 starti)
        (set! cnt (dec cnt))
        (loop [i (inc starti)]
          (let [shifted
                (if (> i (alength arr))
                  js/undefined
                  (let [f (aget arr i)]
                    (if (undefined? f)
                      js/undefined
                      (let [fh   (-hash ^not-native (-key ^not-native f))
                            fni  (unsigned-bit-shift-right fh hash-shifts)
                            fdib (- i fni)]
                        (if (zero? fdib)
                          js/undefined
                          f)))))]
            (aset arr (dec i) shifted)
            (when-not (undefined? shifted)
              (recur (inc i))))))
      js/undefined))

  ICounted
  (-count [_] cnt)

  IIterable
  (-iterator [_]
    (DescriptorMapIter. arr 0 cnt))

  IKVReduce
  (-kv-reduce [_ f init]
    ;; TODO: could inline the iter stuff, but not sure it's worth it
    (let [it (DescriptorMapIter. arr 0 cnt)]
      (loop [init init]
        (if ^boolean (.hasNext it)
          (let [^not-native e (.next it)
                init          (f init (-key e) (-val e))]
            (if (reduced? init)
              @init
              (recur init)))
          init))))

  IFind
  (-find [_ ^not-native k]
    (let [h  (-hash k)
          ni (unsigned-bit-shift-right h hash-shifts)]      ; ni = natural index
      (loop [i ni]
        (let [entry (aget arr i)]
          (if (undefined? entry)
            nil
            (let [^not-native fk (-key ^not-native entry)
                  fh             (-hash fk)]
              (if (and (== h fh) (-equiv k fk))
                entry                                       ; hit
                ;; miss; see if we should keep probing
                (if (< i (alength arr))
                  (recur (inc i))
                  (let [fni  (unsigned-bit-shift-right fh hash-shifts)
                        fdib (- i fni)
                        edib (- i ni)]
                    ;; probing short-circuit: if we encounter an entry whose
                    ;; DIB is smaller than ours, this entry would have been
                    ;; swapped during an insert, so we cannot possibly
                    ;; encounter our desired key
                    #_(assert (>= fdib 0))
                    #_(assert (>= edib 0))
                    (if (< fdib edib)
                      (recur (inc i))
                      nil))))))))))

  ILookup
  (-lookup [o k nf]
    (if-some [entry (-find o k)]
      (-val ^not-native entry)
      nf))
  (-lookup [o k]
    (-lookup o k nil))

  IFn
  (-invoke [o k]
    (-lookup o k))
  (-invoke [o k not-found]
    (-lookup o k not-found))

  ITransientCollection
  (-conj! [o entry]
    (cond
      (map-entry? entry)
      (.insert! o entry)
      (vector? entry)
      (.insert! o (->MapEntry (-nth ^not-native entry 0) (-nth ^not-native entry 1) nil))
      :else
      (reduce rconj! o entry))
    o)
  ;; NOTE: persistent! is deliberately not implemented

  ITransientAssociative
  (-assoc! [o k v]
    (.insert! o (->MapEntry k v nil))
    o)

  ITransientMap
  (-dissoc! [o k]
    (.remove! o k)
    o)

  IEquiv
  (-equiv [dm1 dm2]
    (if (and (instance? DescriptorMap dm2) (== cnt (-count ^not-native dm2)))
      (let [dm1s (alength arr)
            dm2s (alength (.-arr dm2))]
        (if (> dm1s dm2s)
          (descriptor-map-equiv?* dm2 dm1)
          (descriptor-map-equiv?* dm1 dm2)))
      false)))

(defn descriptor-map
  "Return a DescriptorMap: a mutable hash-map with descriptor keys.
  Acts like a transient (assoc!, conj!, dissoc!) but is not persistable.
  Also not hashable or seqable. Implements -equiv for = but only compares =
  with other DescriptorMaps. Use descriptor-map-equiv? to compare with cljs
  collections. Implements -iterator (thus is reducible) and IKVReduce, but
  iterating while modifying will result in
  undefined behavior."
  ([] (descriptor-map 8))
  ([initial-size]
   (assert (pos? initial-size) "Initial size of descriptor-map must be > 0")
   (let [size        (if (zero? (mod initial-size 2))
                       initial-size
                       (next-pow-2 initial-size))
         hash-shifts (hash-shift-for-size size)
         arr         (make-array size)]
     (->DescriptorMap 0 hash-shifts arr))))

(deftype DescriptorSetIter
  [^not-native dmi]
  Object
  (hasNext [_] (.hasNext dmi))
  (next [_] (-key ^not-native (.next dmi))))

(deftype DescriptorSet [^not-native dm]
  Object
  (shrink! [_] (.shrink! dm))
  ICounted
  (-count [_] (-count dm))

  IIterable
  (-iterator [_] (DescriptorSetIter. (-iterator dm)))

  ILookup
  (-lookup [_ k nf] (-lookup dm k nf))
  (-lookup [_ k] (-lookup dm k))

  IFn
  (-invoke [o k]
    (-lookup o k))
  (-invoke [o k not-found]
    (-lookup o k not-found))

  ITransientCollection
  (-conj! [o k]
    (-conj! dm (->MapEntry k k nil))
    o)
  ;; NOTE: persistent! is deliberately not implemented

  ITransientSet
  (-disjoin! [o k]
    (-dissoc! dm k)
    o)

  IEquiv
  (-equiv [ds1 ds2]
    (if (and (instance? DescriptorSet ds2)
          (== (-count ^not-native ds1) (-count ^not-native ds2)))
      (let [dt1  (.-dm ds1)
            dt2  (.-dm ds2)
            dt1s (alength (.-arr dt1))
            dt2s (alength (.-arr dt2))]
        (if (> dt1s dt2s)
          (descriptor-map-equiv?* dt2 dt1)
          (descriptor-map-equiv?* dt1 dt2)))
      false)))

(defn descriptor-set
  "Return a DescriptorSet: a mutable hash-set with descriptor entries.
  Acts like a transient (conj!, disj!) but is not persistable.
  Also not hashable or seqable. Implements -equiv for = but only compares =
  with other DescriptorSet. Use descriptor-set-equiv? to compare with cljs
  collections. Implements -iterator (thus is reducible), but iterating while
  modifying will result in undefined behavior."
  ([] (descriptor-set 8))
  ([initial-size]
   (let [dm (descriptor-map initial-size)]
     (->DescriptorSet dm))))

(defn shrink!
  "Shrink DescriptorMap or DescriptorSet's backing array. Will do nothing if
  already shrunk as much as possible."
  [d-coll]
  (.shrink! d-coll))

