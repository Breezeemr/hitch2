(ns hitch2.selector-impl-registry
  #?(:cljs
     (:require-macros [hitch2.selector-impl-registry]))
  (:require [hitch2.protocols.selector :as selector-proto]
            [cognitect.anomalies :as ca]))

(defonce ^:private REGISTRY (atom {}))

(defn get-registry
  "Return a snapshot of the selector implementation registry.

  A registry snapshot is a map from selector-names to selector-impls."
  []
  @REGISTRY)

(defn get-selector-impl
  "Return a registered selector implementation for a selector name or nil
  if no implementation is registered."
  [selector-name]
  (@REGISTRY selector-name))

(defn reset-registry!
  "Reset the selector implementation registry's state to the supplied
  registry snapshot."
  [v]
  (reset! REGISTRY v))

(defn clear-registry!
  "Clear the selector implementation registry."
  []
  (reset-registry! {}))

(defn register-selector-impl!
  "Register a selector implementation."
  [selector-spec selector-impl]
  (swap! REGISTRY assoc
    (:hitch.selector/name selector-spec)
    selector-impl))

(defn unregister-selector-impl!
  "Remove a selector implementation for the provided selector name."
  [selector-name]
  (swap! REGISTRY dissoc selector-name))

(defn registry-resolver
  "A resolver that looks up selector implementations dynamically from the
  mutable registry or throws if unavailable."
  [selector]
  (let [sname (selector-proto/-sname selector)]
    (or
      (get-selector-impl sname)
      (throw (ex-info (str "Could not find implementation in selector registry " sname)
               {::ca/category  ::ca/not-found
                :selector-name sname
                :selector      selector})))))

#?(:cljs nil
   :clj
   (defmacro def-registered-selector
     "Def symbol with value selector-name and also register its implementation.

  The implementation must be a symbol which resolves to a selector-impl.

  This macro exists for graphs which use the registry-resolver. Use the def-ed
  symbol in select!/dget when using such a graph (instead of a selector-name
  directly) to ensure an implementation exists before using the selector and
  to ensure CLJS advanced-compliation can eliminate dead code."
     [symbol selector-name selector-impl-sym]
     {:pre [(simple-symbol? symbol)
            (simple-symbol? selector-name)
            ;; todo: these resolves don't work in cljs and error out
            ;; and kill the repl
            ;; (some? (resolve selector-name))
            ;; (some? (resolve selector-impl-sym))
            ]}
     `(let []
        (register-selector-impl! ~selector-name ~selector-impl-sym)
        (def ~symbol ~selector-name))))
