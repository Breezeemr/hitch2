(ns hitch2.protocols.tx-manager)

(defprotocol IDependTrack
  (dget-sel! [this data-descriptor nf])
  (finish-tx! [this]))

