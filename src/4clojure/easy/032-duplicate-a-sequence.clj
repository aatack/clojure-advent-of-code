(def __ 
  (fn [sequence]
    (apply concat (map #(list %1 %2) sequence sequence))))

(= (__ [1 2 3]) '(1 1 2 2 3 3))
(= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (__ [44 33]) [44 44 33 33])
