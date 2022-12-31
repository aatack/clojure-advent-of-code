(defn numbers-summing-to [total quantity]
  "Generate all permutations of `quantity` non-negative integers summing
   to a certain total."
  (if (= quantity 1)
    (list [total])
    (for [number (range (inc total))
          others (numbers-summing-to (- total number) (dec quantity))]
      (conj others number))))
