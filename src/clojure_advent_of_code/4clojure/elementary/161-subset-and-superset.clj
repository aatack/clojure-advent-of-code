(def __ #{1 2})

(clojure.set/superset? __ #{2})
(clojure.set/subset? #{1} __)
(clojure.set/superset? __ #{1 2})
(clojure.set/subset? #{1 2} __)
