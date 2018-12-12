(ns hitch2.sel
  (:require [hitch2.protocols.selector :as selector-proto]))

(defrecord ector [name value]
  selector-proto/SelectorName
  (-sname [_]
    name)
  selector-proto/SelectorValue
  (-svalue [_]
    value))

(defn selector? [sel]
  (instance? ector sel))

(defn selector-name [selector]
  (selector-proto/-sname selector))

(defn selector-value [selector]
  (selector-proto/-svalue selector))

(defn sel
  ([selector-spec]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) nil)
     :hitch.selector.spec.canonical-form/map
     (->ector (:hitch.selector/name selector-spec) nil)))
  ([selector-spec a]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) [a])
     :hitch.selector.spec.canonical-form/map
     (let [params  (:hitch.selector.spec/positional-params selector-spec)
           _       (assert (= 1 (count params)))
           [a-key] params]
       (->ector (:hitch.selector/name selector-spec)
         {a-key                a}))))
  ([selector-spec a b]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) [a b])
     :hitch.selector.spec.canonical-form/map
     (let [params        (:hitch.selector.spec/positional-params selector-spec)
           _             (assert (= 2 (count params)))
           [a-key b-key] params]
       (->ector (:hitch.selector/name selector-spec)
         {a-key                a
          b-key                b})
       ))
    )
  ([selector-spec a b c]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) [a b c])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params selector-spec)
           _                   (assert (= 3 (count params)))
           [a-key b-key c-key] params]
       (->ector (:hitch.selector/name selector-spec)
         {a-key                a
          b-key                b
          c-key                c})
       )))
  ([selector-spec a b c d]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) [a b c d])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params selector-spec)
           _                   (assert (= 4 (count params)))
           [a-key b-key c-key d-key] params]
       (->ector (:hitch.selector/name selector-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d}))))

  ([selector-spec a b c d e]
   (case (:hitch.selector.spec/canonical-form selector-spec)
     :hitch.selector.spec.canonical-form/positional
     (->ector (:hitch.selector/name selector-spec) [a b c d e])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params selector-spec)
           _                   (assert (= 5 (count params)))
           [a-key b-key c-key d-key e-key] params]
       (->ector (:hitch.selector/name selector-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d
          e-key                e})))))


(defn map->sel [selector-spec data]
  (case (:hitch.selector.spec/canonical-form selector-spec)
    :hitch.selector.spec.canonical-form/positional
    (let [positional-params (:hitch.selector.spec/positional-params selector-spec)]
      (case (count positional-params)
        0 (->ector (:hitch.selector/name selector-spec) nil)
        1 (let [[a] positional-params]
            (->ector (:hitch.selector/name selector-spec) [(a data)]))
        2 (let [[a b] positional-params]
            (->ector (:hitch.selector/name selector-spec) [ (a data) (b data)]))
        3 (let [[a b c] positional-params]
            (->ector (:hitch.selector/name selector-spec) [(a data) (b data) (c data)]))
        4 (let [[a b c d] positional-params]
            (->ector (:hitch.selector/name selector-spec) [(a data) (b data) (c data) (d data)]))
        5 (let [[a b c d e] positional-params]
            (->ector (:hitch.selector/name selector-spec) [ (a data) (b data) (c data) (d data)  (e data)]))))
    :hitch.selector.spec.canonical-form/map
    (->ector (:hitch.selector/name selector-spec)
      data)))
