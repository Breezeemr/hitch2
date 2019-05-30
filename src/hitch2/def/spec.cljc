(ns hitch2.def.spec
  #?(:cljs (:require-macros [hitch2.def.spec])))

(defn validate-coerce-option [[k v :as option]]
  (if (qualified-keyword? k)
    option
    (case k
      :canonical-form
      [:hitch2.descriptor.spec/canonical-form
       (if (qualified-keyword? v)
         v
         (case v
           :vector :hitch2.descriptor.spec.canonical-form/vector
           :map :hitch2.descriptor.spec.canonical-form/map
           (throw (ex-info
                    "def-descriptor-spec :canonical-form requires fully qualified keys"
                    {:k k
                     :v v}))))]
      :positional-params
      [:hitch2.descriptor.spec/positional-params
       v]
      (throw (ex-info
               "def-descriptor-spec requires fully qualified keys"
               {:k k
                :v v})))))

#?(:cljs nil
   :clj
         (defmacro def-descriptor-spec
           [descriptor-name kind & options]
           {:pre [(simple-symbol? descriptor-name)
                  (keyword? kind)
                  (even? (count options))]}
           (let [dtor-name (symbol
                            (some-> (or
                                      (:name (:ns &env))          ;; targeting CLJS
                                      (ns-name *ns*))             ;; targeting CLJ
                              name)
                            (name descriptor-name))
                 qkind    (if (qualified-keyword? kind)
                            kind
                            (keyword "hitch2.descriptor.spec.kind" (name kind)))
                 eoptions (eduction
                            (comp
                              (partition-all 2)
                              (mapcat validate-coerce-option))
                            options)
                 ]
             `(def ~descriptor-name
                (array-map
                  ~@eoptions
                  :hitch2.descriptor/name ~(list 'quote dtor-name)
                  :hitch2.descriptor.spec/kind ~qkind)))))


" Returns the kind of descriptor or curator.
Should be a keyword for dispatching. Values are from:"
:hitch2.descriptor.kind/var
:hitch2.descriptor.kind/curator
:hitch2.descriptor.kind/sentinel
:hitch2.descriptor.kind/halting
:hitch2.descriptor.kind/process
