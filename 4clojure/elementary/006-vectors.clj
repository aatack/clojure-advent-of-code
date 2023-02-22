(def __ [:a :b :c])

(= __ (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c))
