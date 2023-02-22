(def __
  (fn [sequence]
    (if (= (count sequence) 2)
      (first sequence)
      (recur (rest sequence)))))

(= (__ (list 1 2 3 4 5)) 4)
(= (__ ["a" "b" "c"]) "b")
(= (__ [[1 2] [3 4]]) [1 2])
