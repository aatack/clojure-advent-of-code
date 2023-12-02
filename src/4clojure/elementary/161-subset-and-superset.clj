(def __ #{1 2})

#_{:clj-kondo/ignore [:unresolved-namespace]}
(clojure.set/superset? __ #{2})
#_{:clj-kondo/ignore [:unresolved-namespace]}
(clojure.set/subset? #{1} __)
#_{:clj-kondo/ignore [:unresolved-namespace]}
(clojure.set/superset? __ #{1 2})
#_{:clj-kondo/ignore [:unresolved-namespace]}
(clojure.set/subset? #{1 2} __)
