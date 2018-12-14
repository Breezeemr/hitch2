(ns hitch2.descriptor)

(defrecord Descriptor [name term])

(defn descriptor? [dtor]
  (instance? Descriptor dtor))

(defn dtor
  ([descriptor-spec]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) nil)
     :hitch.selector.spec.canonical-form/map
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) nil)))
  ([descriptor-spec a]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a])
     :hitch.selector.spec.canonical-form/map
     (let [params  (:hitch.selector.spec/positional-params descriptor-spec)
           _       (assert (= 1 (count params)))
           [a-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a}))))
  ([descriptor-spec a b]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b])
     :hitch.selector.spec.canonical-form/map
     (let [params        (:hitch.selector.spec/positional-params descriptor-spec)
           _             (assert (= 2 (count params)))
           [a-key b-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b})
       ))
    )
  ([descriptor-spec a b c]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params descriptor-spec)
           _                   (assert (= 3 (count params)))
           [a-key b-key c-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c})
       )))
  ([descriptor-spec a b c d]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c d])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params descriptor-spec)
           _                   (assert (= 4 (count params)))
           [a-key b-key c-key d-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d}))))

  ([descriptor-spec a b c d e]
   (case (:hitch.selector.spec/canonical-form descriptor-spec)
     :hitch.selector.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c d e])
     :hitch.selector.spec.canonical-form/map
     (let [params              (:hitch.selector.spec/positional-params descriptor-spec)
           _                   (assert (= 5 (count params)))
           [a-key b-key c-key d-key e-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d
          e-key                e})))))


(defn map->dtor [descriptor-spec data]
  (case (:hitch.selector.spec/canonical-form descriptor-spec)
    :hitch.selector.spec.canonical-form/positional
    (let [positional-params (:hitch.selector.spec/positional-params descriptor-spec)]
      (case (count positional-params)
        0 (->Descriptor (:hitch2.descriptor/name descriptor-spec) nil)
        1 (let [[a] positional-params]
            (->Descriptor (:hitch2.descriptor/name descriptor-spec) [(a data)]))
        2 (let [[a b] positional-params]
            (->Descriptor (:hitch2.descriptor/name descriptor-spec) [ (a data) (b data)]))
        3 (let [[a b c] positional-params]
            (->Descriptor (:hitch2.descriptor/name descriptor-spec) [(a data) (b data) (c data)]))
        4 (let [[a b c d] positional-params]
            (->Descriptor (:hitch2.descriptor/name descriptor-spec) [(a data) (b data) (c data) (d data)]))
        5 (let [[a b c d e] positional-params]
            (->Descriptor (:hitch2.descriptor/name descriptor-spec) [ (a data) (b data) (c data) (d data)  (e data)]))))
    :hitch.selector.spec.canonical-form/map
    (->Descriptor (:hitch2.descriptor/name descriptor-spec)
      data)))
