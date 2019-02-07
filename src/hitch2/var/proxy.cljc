(ns hitch2.var.proxy)

(defn var-proxy-impl [proxy-curator]
  {:hitch2.descriptor.impl/kind :hitch2.descriptor.kind/var
   :hitch2.descriptor.impl/get-curator
                                (fn [sel]
                                  proxy-curator)})
