(def __ [5 4 3 2 1])

(= __ ((fn foo [x] (when (> x 0) (conj (foo (dec x)) x))) 5))
