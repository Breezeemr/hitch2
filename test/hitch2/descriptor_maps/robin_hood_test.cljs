(ns hitch2.descriptor-maps.robin-hood-test
  (:require [hitch2.descriptor-maps.robin-hood :as rh]
            [clojure.test :refer [deftest is testing]]))

(deftest descriptor-map-assoc
  (let [t          (rh/descriptor-map)
        s          1000
        x          (into #{} (map #(do [[%] [%]]) (range s)))
        _          (run! #(assoc! t [%] [%]) (range s))
        tv         (into [] t)
        ta         (into [] (filter some?) (.-arr t))
        not-founds (keep
                     #(let [k [%]
                            e [k k]
                            f (find t k)]
                        (when-not (= e f)
                          [e f]))
                     (range s))]
    (is (= ta tv) "Underlying array should correspond to iterator")
    (is (= (set tv) x) "All members must be present from iteraton")
    (is (empty? not-founds)
      "Each element should be findable")
    (testing "repeated assocs"
      (run! #(assoc! t [%] [%]) (range s))
      (let [tv (into [] t)
            ta (into [] (filter some?) (.-arr t))]
        (is (= ta tv) "Underlying array should correspond to iterator after repeated assoc")
        (is (= (set tv) x) "All members must be present from iteraton after repeated assoc")
        (is (nil? (some
                    #(let [k [%]
                           e [k k]
                           f (find t k)]
                       (when-not (= e f)
                         [e f]))
                    (range s)))
          "Each element should be findable after repeated assoc")))))

(deftest descriptor-map-assoc-dissoc-roundtrip
  (let [t (rh/descriptor-map 12)]
    (run! #(assoc! t [%] [%]) (range 10))
    (run! #(dissoc! t [%]) (range 10))
    (is (= (count t) 0))
    (is (= [] (into [] (filter some?) (.-arr t))))
    (is (= [] (into [] t)))))

(deftest growing-equivalent-to-presized
  (let [t1 (rh/descriptor-map 8)
        t2 (rh/descriptor-map 64)
        t3 (rh/descriptor-map 63)
        s  49]
    (is (= (alength (.-arr t1)) 8))
    (is (= (alength (.-arr t2)) 64))
    (is (= (alength (.-arr t3)) 64) "Initial size hint should round to next power of 2")
    (run! #(assoc! t1 [%] [%]) (range s))
    (run! #(assoc! t2 [%] [%]) (range s))
    (testing "backing arrays are the same between grown and pre-sized maps"
      (is (= (alength (.-arr t1)) (alength (.-arr t2))))
      (is (= (into [] (.-arr t1)) (into [] (.-arr t2)))))))

(deftest descriptor-map-equiv
  (let [m  {[1] 1 [2] 2 [3] 3 [4] 4}
        m1 (rh/descriptor-map)
        m1 (reduce (fn [m1 [k v]] (assoc! m1 k v)) m1 m)]
    (testing "equiv DescriptorMap and PAM should be equiv in various ways"
      (is (rh/descriptor-map-equiv?* m1 m))
      (is (rh/descriptor-map-equiv? m1 m))
      (is (rh/descriptor-map-equiv? m m1))
      (is (not (= m1 m)) "Descriptor maps may not = non-DMs")
      (is (not (= m m1)) "Descriptor maps may not = non-DMs"))

    (testing "equiv DescriptorMap and PAM should be not-equiv in various ways"
      (let [m (dissoc m [3])]
        (is (not (rh/descriptor-map-equiv?* m1 m)))
        (is (not (rh/descriptor-map-equiv? m1 m)))
        (is (not (rh/descriptor-map-equiv? m m1)))
        (is (not (= m1 m)))
        (is (not (= m m1)))))
    (testing "formerly not-equiv DM and PAM should re-equiv if DM changed"
      (let [m  (dissoc m [3])
            m1 (dissoc! m1 [3])]
        (is (rh/descriptor-map-equiv?* m1 m))
        (is (rh/descriptor-map-equiv? m1 m))
        (is (rh/descriptor-map-equiv? m m1))
        (is (not (= m1 m)))
        (is (not (= m m1)))))))

(deftest descriptor-map-conj
  (testing "Conj! of a single entry should act like assoc!"
    (let [m1 (rh/descriptor-map)
          m1 (conj! m1 [[1] 1])
          m2 (rh/descriptor-map)
          m2 (assoc! m2 [1] 1)]
      (is (= (find m1 [1]) (find m2 [1])))
      (is (rh/descriptor-map-equiv? m1 {[1] 1}))
      (is (rh/descriptor-map-equiv? m2 {[1] 1}))
      (is (= m1 m2)))))

(deftest descriptor-map-conj-seqs
  (let [m1 (rh/descriptor-map)
        m1 (reduce conj! m1 (map #(do [[%] %]) (range 100)))
        m2 (rh/descriptor-map)
        m2 (apply conj! m2 (map #(do [[%] %]) (range 100)))
        m3 (rh/descriptor-map)
        m3 (apply conj! m3 (map #(->MapEntry [%] % nil) (range 100)))
        m4 (rh/descriptor-map)
        m4 (conj! m4 (into {} (map #(do [[%] %]) (range 100))))]
    (is (rh/descriptor-map-equiv?* m1 m2))
    (is (rh/descriptor-map-equiv?* m2 m3))
    (is (rh/descriptor-map-equiv?* m3 m4))))

(deftest descriptor-map-kvreduce
  (let [m1        (rh/descriptor-map)
        m1        (reduce conj! m1 (map #(do [[%] %]) (range 100)))
        kvsum     (fn [s [k] v] (+ s k v))
        s-control (->> (into {} (map #(do [[%] %]) (range 100)))
                       (reduce-kv kvsum 0))
        s1        (reduce-kv kvsum 0 m1)]
    (is (= s1 s-control))))

(deftest descriptor-map-shrinking
  ;; m1 will gain a bunch of keys then lose 3/4 of them
  ;; m2 will gain only the keys that m1 has in the end
  (let [m1 (rh/descriptor-map 8)
        m1 (reduce conj! m1 (map #(do [[%] %]) (range 100)))
        m1 (reduce dissoc! m1 (map vector (range 78)))
        m2 (rh/descriptor-map 8)
        m2 (reduce conj! m2 (map #(do [[%] %]) (range 78 100)))]
    (is (= m1 m2))
    (is (> (alength (.-arr m1)) (alength (.-arr m2))))
    (rh/shrink! m1)
    (is (== (alength (.-arr m1)) (alength (.-arr m2))))
    (is (= m1 m2))))

(deftest descriptor-set-conj-disj
  (let [s1 (rh/descriptor-set)
        s1 (conj! s1 [1])]
    (is (contains? s1 [1]))
    (is (= (get s1 [1]) [1]))
    (is (= (s1 [1]) [1]))
    (let [s1 (disj! s1 [1])]
      (is (not (contains? s1 [1])))
      (is (not (= (get s1 [1]) [1])))
      (is (not (= (s1 [1]) [1]))))))

(deftest descriptor-set-equiv
  (let [s  #{[1] [2] [3] [4]}
        s1 (rh/descriptor-set)
        s1 (reduce conj! s1 s)
        s2 (rh/descriptor-set)
        s2 (reduce conj! s2 s)]
    (testing "DescriptorSets should be equiv to each other in various ways"
      (is (= s1 s2))
      (is (= s2 s1))
      (is (rh/descriptor-set-equiv? s1 s2))
      (is (rh/descriptor-set-equiv? s2 s1)))
    (testing "DescriptorSets should be not-equiv to each other in various ways"
      (disj! s2 [2])
      (is (not (= s1 s2)))
      (is (not (= s2 s1)))
      (is (not (rh/descriptor-set-equiv? s1 s2)))
      (is (not (rh/descriptor-set-equiv? s2 s1))))

    (testing "equiv DescriptorSet and PHS should be equiv in various ways"
      (is (rh/descriptor-set-equiv? s1 s))
      (is (rh/descriptor-set-equiv? s s1))
      (is (not (= s1 s)) "Descriptor maps may not = non-DMs")
      (is (not (= s s1)) "Descriptor maps may not = non-DMs"))

    (testing "equiv DescriptorSet and PHS should be not-equiv in various ways"
      (let [s (disj s [3])]
        (is (not (rh/descriptor-set-equiv? s1 s)))
        (is (not (rh/descriptor-set-equiv? s s1)))
        (is (not (= s1 s)))
        (is (not (= s s1)))))
    (testing "formerly not-equiv DM and PHS should re-equiv if DM changed"
      (let [s  (disj s [3])
            s1 (disj! s1 [3])]
        (is (rh/descriptor-set-equiv? s1 s))
        (is (rh/descriptor-set-equiv? s s1))
        (is (not (= s1 s)))
        (is (not (= s s1)))))))

(deftest descriptor-set-reduce
  (let [s1        (rh/descriptor-set)
        s1        (reduce conj! s1 (map vector (range 100)))
        ksum      (fn [s [k]] (+ s k))
        s-control (->> (into #{} (map vector (range 100)))
                       (reduce ksum 0))
        s1        (reduce ksum 0 s1)]
    (is (= s1 s-control))))

;; On a throttled CPU I see some memory savings but not really any time savings
;; for robinhood cache vs transient
;; These benches are not using descriptors and are not integrated into the graph,
;; so maybe there is some possible win anyway, but it doesn't seem like the
;; slam-dunk I thought it would be.
#_
(do
  (simple-benchmark [m {} r (range 5000)]
    (into m (map #(do [[%] %])) r)
    100)


  (simple-benchmark [m (transient {}) r (range 5000)]
    (reduce (fn [m i] (assoc! m [i] i)) m r)
    100)


  (simple-benchmark [m (rh/descriptor-map) r (range 5000)]
    (let [_ (.mark js/performance "dmconj:s")
          x (reduce (fn [m i] (assoc! m [i] i)) m r)
          _ (.mark js/performance "dmconj:e")
          _ (.measure js/performance "dmconj" "dmconj:s" "dmconj:e")]
      x)
    100)

  (simple-benchmark [m (rh/descriptor-map)
                     r (range 5000)
                     m (reduce (fn [m i] (assoc! m [i] i)) m r)]
    (let [_ (.mark js/performance "dmlook:s")
          x (into [] (map (comp m vector)) r)
          _ (.mark js/performance "dmlook:e")
          _ (.measure js/performance "dmlook" "dmlook:s" "dmlook:e")]
      x)
    100)

  (simple-benchmark [m (transient {})
                     r (range 5000)
                     m (reduce (fn [m i] (assoc! m [i] i)) m r)]
    (let [_ (.mark js/performance "tmlook:s")
          x (into [] (map (comp m vector)) r)
          _ (.mark js/performance "tmlook:e")
          _ (.measure js/performance "tmlook" "tmlook:s" "tmlook:e")]
      x)
    100)

  (simple-benchmark [m (rh/descriptor-map 5000) r (range 5000)]
    (reduce (fn [m i] (assoc! m [i] i)) m r)
    100))
