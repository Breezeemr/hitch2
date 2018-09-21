(ns hitch2.sentinels)

(defonce NOT-FOUND-SENTINEL
  (reify
    #?@(:clj [Object
              (toString [_] "#<NOT-FOUND-SENTINEL>")]
        :cljs
        [IPrintWithWriter
         (-pr-writer [_ writer opts]
                     (-write writer "#<NOT-FOUND-SENTINEL>"))])))

(defonce NOT-IN-GRAPH-SENTINEL
  (reify
    #?@(:clj  [Object
               (toString [_] "#<NOT-IN-GRAPH-SENTINEL>")]
        :cljs [IPrintWithWriter
               (-pr-writer [_ writer opts]
                           (-write writer "#<NOT-IN-GRAPH-SENTINEL>"))])))
