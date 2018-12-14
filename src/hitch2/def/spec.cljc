(ns hitch2.def.spec
  #?(:cljs (:require-macros [hitch2.def.spec])))


#?(:cljs nil
   :clj
         (defmacro def-descriptor-spec
           [selector-name kind & options]
           {:pre [(simple-symbol? selector-name)
                  (keyword? kind)]}
           (let [sel-name (symbol
                            (some-> (or
                                      (:name (:ns &env))          ;; targeting CLJS
                                      (ns-name *ns*))             ;; targeting CLJ
                              name)
                            (name selector-name))
                 qkind    (if (qualified-keyword? kind)
                            kind
                            (keyword "hitch2.descriptor.spec.kind" (name kind)))]
             `(def ~selector-name
                (array-map
                  ~@options
                  :hitch2.descriptor/name ~(list 'quote sel-name)
                  :hitch2.descriptor.spec/kind ~qkind)))))


" Returns the kind of selector or curator.
Should be a keyword for dispatching. Values are from:"
:hitch.selector.kind/var
:hitch.selector.kind/curator
:hitch.selector.kind/sentinel
:hitch.selector.kind/halting
