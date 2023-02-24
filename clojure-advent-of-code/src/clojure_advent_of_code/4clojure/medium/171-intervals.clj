(defn __ [integers]
  (letfn [(partition-when [function sequence]
            (let [[completed awaiting]
                  (reduce (fn [[completed awaiting] item]
                            (if (or (empty? awaiting)
                                    (not (function (last awaiting) item)))
                              [completed (conj awaiting item)]
                              [(conj completed awaiting) [item]]))
                          [[] []]
                          sequence)]
              (if (empty? awaiting)
                completed
                (conj completed awaiting))))]

    (->> integers
         sort
         (partition-when #(> %2 (inc %1)))
         (map #(list (first %) (last %))))))

(= (__ [1 2 3]) [[1 3]])
(= (__ [10 9 8 1 2 3]) [[1 3] [8 10]])
(= (__ [1 1 1 1 1 1 1]) [[1 1]])
(= (__ []) [])
(= (__ [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
   [[1 4] [6 6] [9 11] [13 17] [19 19]])
