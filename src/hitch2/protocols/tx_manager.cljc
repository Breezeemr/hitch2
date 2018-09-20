(ns hitch2.protocols.tx-manager)

(defprotocol IDependTrack
  (dget-sel! [this data-selector nf])
  (finish-tx! [this]))

