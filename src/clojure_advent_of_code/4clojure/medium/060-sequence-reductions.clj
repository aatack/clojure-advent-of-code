(defn custom-reductions
  ([accumulator sequence]
   (custom-reductions accumulator
                      (first sequence)
                      (rest sequence)))
  ([accumulator initial sequence]
   (lazy-seq (cons initial
                   (when (not-empty sequence)
                     (custom-reductions accumulator
                                        (accumulator initial (first sequence))
                                        (rest sequence)))))))

(def __ custom-reductions)

(= (take 5 (__ + (range))) [0 1 3 6 10])
(= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
(= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
