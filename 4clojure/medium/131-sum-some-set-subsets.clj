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
      (boolean (some (fn [value]
                       (every? #(% value) others))
                     (first sums))))))

(= true (__ #{-1 1 99}
            #{-2 2 888}
            #{-3 3 7777}))
(= false (__ #{1}
             #{2}
             #{3}
             #{4}))
(= true  (__ #{1}))
(= false (__ #{1 -3 51 9}
             #{0}
             #{9 2 81 33}))
(= true  (__ #{1 3 5}
             #{9 11 4}
             #{-3 12 3}
             #{-3 4 -2 10}))
(= false (__ #{-1 -2 -3 -4 -5 -6}
             #{1 2 3 4 5 6 7 8 9}))
(= true  (__ #{1 3 5 7}
             #{2 4 6 8}))
(= true  (__ #{-1 3 -5 7 -9 11 -13 15}
             #{1 -3 5 -7 9 -11 13 -15}
             #{1 -1 2 -2 4 -4 8 -8}))
(= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
             #{10 -9 8 -7 6 -5 4 -3 2 -1}))
