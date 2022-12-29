(def __ 4)

(contains? #{4 5 6} __)
(contains? [1 1 1 1 1] __)
(contains? {4 :a 2 :b} __)
(not (contains? [1 2 4] __))
