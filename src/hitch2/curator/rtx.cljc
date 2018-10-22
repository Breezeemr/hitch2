(ns hitch2.curator.rtx
  (:require [hitch2.tx-manager.halting :as halting]
            [hitch2.protocols.tx-manager :as tx-manager]
            [hitch2.halt :as halt]
            [hitch2.protocols.graph-manager :as g]
            [hitch2.curator.react-hook :as rh]))

(defn qui-hitch
  ([graph unresolved c render-fn value services]
   (let [gv              (g/-get-graph graph)
         rtx             (halting/halting-manager gv)
         result          (halt/maybe-halt
                          (render-fn value rtx services)
                          unresolved)
         focus-selectors (tx-manager/finish-tx! rtx)]
     (g/-transact! graph rh/react-hooker
                   [:reset-component-parents c focus-selectors])
     result))
  ([graph unresolved c render-fn value meta services]
   (let [gv              (g/-get-graph graph)
         rtx             (halting/halting-manager gv)
         result          (halt/maybe-halt
                          (render-fn value rtx meta services)
                          unresolved)
         focus-selectors (tx-manager/finish-tx! rtx)]
     (g/-transact! graph rh/react-hooker
                   [:reset-component-parents c focus-selectors])
     result)))
