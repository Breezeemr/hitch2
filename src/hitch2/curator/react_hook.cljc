(ns hitch2.curator.react-hook
  (:require [hitch2.protocols.curator :as machine-proto]
            [hitch2.protocols.graph-manager :as graph-proto]
            [hitch2.sentinels :refer [NOT-FOUND-SENTINEL]]
            [hitch2.protocols.selector :as sel-proto
             :refer [def-selector-spec]]
            [hitch2.selector-impl-registry :as reg]))
