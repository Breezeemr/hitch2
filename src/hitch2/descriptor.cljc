(ns hitch2.descriptor)

(defrecord Descriptor [name term])

(defn descriptor? [dtor]
  (instance? Descriptor dtor))

(defn dtor
  ([descriptor-spec]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) nil)
     :hitch2.descriptor.spec.canonical-form/map
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) nil)))
  ([descriptor-spec a]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a])
     :hitch2.descriptor.spec.canonical-form/map
     (let [params  (:hitch2.descriptor.spec/positional-params descriptor-spec)
           _       (assert (= 1 (count params)))
           [a-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a}))))
  ([descriptor-spec a b]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b])
     :hitch2.descriptor.spec.canonical-form/map
     (let [params        (:hitch2.descriptor.spec/positional-params descriptor-spec)
           _             (assert (= 2 (count params)))
           [a-key b-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b})
       ))
    )
  ([descriptor-spec a b c]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c])
     :hitch2.descriptor.spec.canonical-form/map
     (let [params              (:hitch2.descriptor.spec/positional-params descriptor-spec)
           _                   (assert (= 3 (count params)))
           [a-key b-key c-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c})
       )))
  ([descriptor-spec a b c d]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c d])
     :hitch2.descriptor.spec.canonical-form/map
     (let [params              (:hitch2.descriptor.spec/positional-params descriptor-spec)
           _                   (assert (= 4 (count params)))
           [a-key b-key c-key d-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d}))))

  ([descriptor-spec a b c d e]
   (case (:hitch2.descriptor.spec/canonical-form  descriptor-spec)
     :hitch2.descriptor.spec.canonical-form/positional
     (->Descriptor (:hitch2.descriptor/name descriptor-spec) [a b c d e])
     :hitch2.descriptor.spec.canonical-form/map
     (let [params              (:hitch2.descriptor.spec/positional-params descriptor-spec)
           _                   (assert (= 5 (count params)))
           [a-key b-key c-key d-key e-key] params]
       (->Descriptor (:hitch2.descriptor/name descriptor-spec)
         {a-key                a
          b-key                b
          c-key                c
          d-key                d
          e-key                e})))))


(defn ->dtor [descriptor-spec data]
  ;TODO validate spec
  (->Descriptor (:hitch2.descriptor/name descriptor-spec)
    data))
