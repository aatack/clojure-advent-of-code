(defn custom-reverse [sequence]
  (if (empty? sequence)
    []
    (conj (custom-reverse (rest sequence)) (first sequence))))

(def __ custom-reverse)

(= (__ [1 2 3 4 5]) [5 4 3 2 1])
(= (__ (sorted-set 5 7 2 7)) '(7 5 2))
(= (__ [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]])
