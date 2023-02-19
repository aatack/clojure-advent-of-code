(defn __ [& input-sets]
  (letfn [(k-combinations [length items]
            (case length
              0 #{}
              1 (set (map #(set [%]) items))
              (set (for [outer (k-combinations (dec length) items)
                         inner items
                         :when (not (outer inner))]
                     (conj outer inner)))))

          (subsets [input-set]
            (apply concat (map #(k-combinations % input-set)
                               (-> input-set count inc range))))]

    (subsets #{0 1 2})))

(__ 1)
