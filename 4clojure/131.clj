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
                               (-> input-set count inc range))))

          (subset-sums [input-set]
            (set (map #(apply + %) (subsets input-set))))]

    (let [sums (map subset-sums input-sets)
          others (rest sums)]
      (some (fn [value]
              (every? #(% value) others))
            (first sums)))))

(= true (__ #{-1 1 99}
            #{-2 2 888}
            #{-3 3 7777}))
