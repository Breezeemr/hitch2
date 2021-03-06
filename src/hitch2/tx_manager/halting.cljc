(ns hitch2.tx-manager.halting
  (:require [hitch2.protocols.tx-manager :as tx-manager]
            [hitch2.halt :as halt]))


(deftype HaltingManager [graph-value
                         #?(:cljs    ^:mutable requests
                            :default ^:unsynchronized-mutable requests)
                         #?(:cljs    ^:mutable blocking
                            :default ^:unsynchronized-mutable blocking)]
  tx-manager/IDependTrack
  (dget-sel!  [this data-descriptor nf]
    (set! requests (conj! requests data-descriptor))
    (get graph-value data-descriptor nf))
  (finish-tx! [this]
    (let [reqs (persistent! requests)]
      (set! requests (transient #{}))
      reqs))
  tx-manager/IBlockingLoad
  (get-blocking [this] blocking)
  (set-blocking! [this descriptor] (set! blocking descriptor)))

(defn halting-manager [graph-value]
  (HaltingManager. graph-value (transient #{}) nil))
