(ns hitch2.def.halting
  #?(:cljs (:require-macros hitch2.def.halting))
  (:require [hitch2.halt :as halt]
            [hitch2.def.spec
             :refer [def-descriptor-spec]]
            [hitch2.descriptor-impl-registry :as reg]))

(defn- cljs-target? [env]
  (some? (:ns env)))

(defn- param-names [binding-form]
  (mapv (fn [x]
          (assert (not= x '&)
            "Variadic parameters are not allowed on defdescriptor.")
          (cond
            (symbol? x) x
            (and (map? x) (:as x)) (:as x)
            (and (vector? x)
              (>= (count x) 2)
              (= (-> x pop peek) :as)) (peek x)
            :else
            (throw (ex-info "Every parameter to defhalting must have a name, either directly or with a top-level :as"
                     {:binding-form binding-form
                      :bad-param x}))))
    binding-form))

(defn make-eval-arg-binding [x]
  (assert (not= x '&)
    "Variadic parameters are not allowed on defdescriptor.")
  (cond
    (symbol? x) [x (keyword x)]
    (and (map? x) (:as x)) [x (keyword (:as x))]
    (and (vector? x)
      (>= (count x) 2)
      (= (-> x pop peek) :as)) [x (keyword (peek x))]
    :else
    (throw (ex-info "Every parameter to defhalting must have a name, either directly or with a top-level :as"
             {:bad-param x})))
  )
(defn make-eval-binding-form [graph-symbol record-field-names ]
  [graph-symbol
   {(into {}
      (map make-eval-arg-binding)
      record-field-names)
    :term}])

(defn -def-halting [name constructor-binding-forms body]
  (let [input              (rest constructor-binding-forms)
        record-field-names (param-names input)
        eval-fn-name       (symbol (str name "-eval-fn"))
        slot-eval-fn-name  (symbol (str name "-slot-eval-fn"))
        impl               (symbol (str name "-impl"))
        spec               (symbol (str name "-spec"))]
    `(do
       (hitch2.def.spec/def-descriptor-spec
         ~spec
         :not-curator
         :hitch2.descriptor.spec/canonical-form
         :hitch2.descriptor.spec.canonical-form/map
         :hitch2.descriptor.spec/positional-params
         ~(mapv #(keyword (namespace %) (clojure.core/name %)) record-field-names))
       (defn ~eval-fn-name ~(make-eval-binding-form (first constructor-binding-forms) input) ~@body)
       ;; This is Francis' descriptor. Halting fn signature is different:
       ;; (fn [dt MAP-LIKE-descriptor-WITH-NON-POS-ENTRIES pos1 pos2 ...] ...)
       ;; For now we just ignore the second arg
       (defn ~slot-eval-fn-name
         ~(into [(first constructor-binding-forms) '_]
            (rest constructor-binding-forms))
         ~@body)
       (def ~impl
         {:hitch2.descriptor.impl/kind                  :hitch2.descriptor.kind/halting
          :hitch2.descriptor.impl/halting               ~eval-fn-name
          :hitch2.descriptor.impl/halting-slot-descriptor ~slot-eval-fn-name})
       (hitch2.descriptor-impl-registry/def-registered-descriptor
         ~name  ~spec ~impl))))

(defmacro defhalting [name constructor-binding-forms & body]
  (-def-halting name constructor-binding-forms body))
