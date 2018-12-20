(ns hitch2.descriptor-impl-registry
  #?(:cljs
     (:require-macros [hitch2.descriptor-impl-registry]))
  (:require [cognitect.anomalies :as ca]))

(defonce ^:private REGISTRY (atom {}))

(defn get-registry
  "Return a snapshot of the descriptor implementation registry.

  A registry snapshot is a map from descriptor-names to descriptor-impls."
  []
  @REGISTRY)

(defn get-descriptor-impl
  "Return a registered descriptor implementation for a descriptor name or nil
  if no implementation is registered."
  [descriptor-name]
  (@REGISTRY descriptor-name))

(defn reset-registry!
  "Reset the descriptor implementation registry's state to the supplied
  registry snapshot."
  [v]
  (reset! REGISTRY v))

(defn clear-registry!
  "Clear the descriptor implementation registry."
  []
  (reset-registry! {}))

(defn register-descriptor-impl!
  "Register a descriptor implementation."
  [descriptor-spec descriptor-impl]
  (swap! REGISTRY assoc
    (:hitch2.descriptor/name descriptor-spec)
    descriptor-impl))

(defn unregister-descriptor-impl!
  "Remove a descriptor implementation for the provided descriptor name."
  [descriptor-name]
  (swap! REGISTRY dissoc descriptor-name))

(defn registry-resolver
  "A resolver that looks up descriptor implementations dynamically from the
  mutable registry or throws if unavailable."
  [descriptor]
  (let [sname (:name descriptor)]
    (or
      (get-descriptor-impl sname)
      (throw (ex-info (str "Could not find implementation in descriptor registry " sname)
               {::ca/category  ::ca/not-found
                :descriptor-name sname
                :descriptor      descriptor})))))

#?(:cljs nil
   :clj
   (defmacro def-registered-descriptor
     "Def symbol with value descriptor-name and also register its implementation.

  The implementation must be a symbol which resolves to a descriptor-impl.

  This macro exists for graphs which use the registry-resolver. Use the def-ed
  symbol in select!/dget when using such a graph (instead of a descriptor-name
  directly) to ensure an implementation exists before using the descriptor and
  to ensure CLJS advanced-compliation can eliminate dead code."
     [symbol descriptor-name descriptor-impl-sym]
     {:pre [(simple-symbol? symbol)
            (simple-symbol? descriptor-name)
            ;; todo: these resolves don't work in cljs and error out
            ;; and kill the repl
            ;; (some? (resolve descriptor-name))
            ;; (some? (resolve descriptor-impl-sym))
            ]}
     `(let []
        (register-descriptor-impl! ~descriptor-name ~descriptor-impl-sym)
        (def ~symbol ~descriptor-name))))
