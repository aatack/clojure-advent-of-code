(def __
  (fn [sequence]
    (if (= (count sequence) 1)
      (first sequence)
      (recur (rest sequence)))))

(= (__ [1 2 3 4 5]) 5)
(= (__ '(5 4 3)) 3)
(= (__ ["b" "c" "d"]) "d")
