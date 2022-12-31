(defn numbers-summing-to [total quantity]
  "Generate all permutations of `quantity` non-negative integers summing
   to a certain total."
  (cond
    (or (= quantity 0) (= total 0)) ()
    (= quantity 1) (list [total])
    :else (for [number (range (inc total))
                others (numbers-summing-to (- total number) (dec quantity))]
            (conj others number))))

(defn variable-length-partition [sequence lengths]
  (when (not-empty lengths)
    (let [quantity (first lengths)]
      (cons (take quantity sequence)
            (variable-length-partition (drop quantity sequence)
                                       (rest lengths))))))

(defn dynamic-for [item-lists]
  ;; TODO: implement this properly
  [item-lists])

(defn possible-expressions [numbers]
  (if (= (count numbers) 1)
    (first numbers)
    (for [operator [+ - / *]
          ;; TODO: also include permutations in the loop
          partitions (map (fn [sequence] (remove #(= % 0) sequence))
                          (numbers-summing-to (count numbers)
                                              (count numbers)))
          :when (> (count partitions) 1)
          :let [groups (variable-length-partition numbers partitions)]
          expressions (dynamic-for (map possible-expressions groups))]
      (cons operator expressions))))
