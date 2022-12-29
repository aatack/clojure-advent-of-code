(def __ [1 4 7 10 13])

(= __ (take 5 (iterate #(+ 3 %) 1)))
