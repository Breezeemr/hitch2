(ns com.breezeehr.hitch2.compile-java
  (:require [badigeon.javac :refer [javac]]))


(defn -main [& args]
  (javac "src-java"))
