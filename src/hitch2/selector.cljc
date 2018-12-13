(ns hitch2.selector
  #?(:cljs (:require-macros hitch2.selector))
  (:require [hitch2.halt :as halt]
            [hitch2.protocols.selector :as selector-proto
             :refer [def-selector-spec]]
            [hitch2.selector-impl-registry :as reg]))

(defn- cljs-target? [env]
  (some? (:ns env)))

(defn- sel-constructor
  [name impl-symbol param-names]
  (let [param-count (count param-names)
        record-selector-symbol
        (symbol "hitch2.protocols.selector" (str "->Selector" param-count))]
    `(defn ~name ~param-names
       (~record-selector-symbol ~impl-symbol ~@param-names))))

(defn- param-names [binding-form]
  (mapv (fn [x]
          (assert (not= x '&)
            "Variadic parameters are not allowed on defselector.")
          (cond
            (symbol? x) x
            (and (map? x) (:as x)) (:as x)
            (and (vector? x)
              (>= (count x) 2)
              (= (-> x pop peek) :as)) (peek x)
            :else
            (throw (ex-info "Every parameter to defselector must have a name, either directly or with a top-level :as"
                     {:binding-form binding-form
                      :bad-param x}))))
    binding-form))

(defn make-eval-arg-binding [x]
  (assert (not= x '&)
    "Variadic parameters are not allowed on defselector.")
  (cond
    (symbol? x) [x (keyword x)]
    (and (map? x) (:as x)) [x (keyword (:as x))]
    (and (vector? x)
      (>= (count x) 2)
      (= (-> x pop peek) :as)) [x (keyword (peek x))]
    :else
    (throw (ex-info "Every parameter to defselector must have a name, either directly or with a top-level :as"
             {:bad-param x})))
  )
(defn make-eval-binding-form [graph-symbol record-field-names ]
  [graph-symbol
   {(into {}
      (map make-eval-arg-binding)
      record-field-names)
    :value}])

(defn tylers-def-selector [name constructor-binding-forms body]
  (let [input              (rest constructor-binding-forms)
        record-field-names (param-names input)
        eval-fn-name       (symbol (str name "-eval-fn"))
        slot-eval-fn-name  (symbol (str name "-slot-eval-fn"))
        impl               (symbol (str name "-impl"))
        spec               (symbol (str name "-spec"))]
    `(do
       (hitch2.protocols.selector/def-selector-spec
         ~spec
         :not-machine
         :hitch.selector.spec/canonical-form
         :hitch.selector.spec.canonical-form/map
         :hitch.selector.spec/positional-params
         ~(mapv #(keyword (namespace %) (clojure.core/name %)) record-field-names))
       (defn ~eval-fn-name ~(make-eval-binding-form (first constructor-binding-forms) input) ~@body)
       ;; This is Francis' selector. Halting fn signature is different:
       ;; (fn [dt MAP-LIKE-SELECTOR-WITH-NON-POS-ENTRIES pos1 pos2 ...] ...)
       ;; For now we just ignore the second arg
       (defn ~slot-eval-fn-name
         ~(into [(first constructor-binding-forms) '_]
            (rest constructor-binding-forms))
         ~@body)
       (def ~impl
         {:hitch.selector.impl/kind                  :hitch.selector.kind/halting
          :hitch.selector.impl/halting               ~eval-fn-name
          :hitch.selector.impl/halting-slot-selector ~slot-eval-fn-name}
         #_(reify
           hitch2.protocols.selector/ImplementationKind
           (~'-imp-kind [~'_] :hitch.selector.kind/halting)
           hitch2.protocols.selector/HaltingImplementation
           (~'-get-halting-fn [~'sel]
             ~eval-fn-name)))
       (hitch2.selector-impl-registry/def-registered-selector
         ~name  ~spec ~impl))))

(defmacro defselector [name constructor-binding-forms & body]
  (tylers-def-selector name constructor-binding-forms body))
