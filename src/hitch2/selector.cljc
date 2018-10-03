(ns hitch2.selector
  #?(:cljs (:require-macros hitch2.selector))
  (:require [hitch2.halt :as halt]
            [hitch2.protocols.selector :as selector-proto]
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

(defmacro defselector [name constructor-binding-forms & body]
  (let [record-field-names   (param-names (rest constructor-binding-forms))
        eval-fn-name         (symbol (str name "-eval-fn"))
        impl              (symbol (str name "-impl"))
        impl-name         (keyword name)
        sel-var           (symbol (str name "-sel-var"))]
    `(do
       (defn ~eval-fn-name ~constructor-binding-forms ~@body)
       (def ~impl
         (reify
           hitch2.protocols.selector/ImplementationKind
           (~'-imp-kind [~'_] :hitch.selector.kind/halting)
           hitch2.protocols.selector/HaltingImplementation
           (~'-get-halting-fn [~'sel]
            ~eval-fn-name)))
       (hitch2.selector-impl-registry/def-registered-selector
         ~sel-var  ~impl-name ~impl)
       ~(sel-constructor name impl-name record-field-names))))
