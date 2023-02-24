(defn __ [start end]
  (letfn [(explore [value]
            (set (concat [(* value 2) (+ value 2)]
                         (if (even? value) [(/ value 2)] []))))]

    (loop [unexplored [[start 0]]
           explored #{start}]
      (let [sorted (sort-by second unexplored)
            [node length] (first sorted)
            nodes (rest sorted)]
        (if (= node end)
          (inc length)
          (let [additions (apply disj (explore node) explored)]
            (recur (concat nodes (map #(vector % (inc length)) additions))
                   (concat explored additions))))))))

(= 1 (__ 1 1))
(= 3 (__ 3 12))
(= 3 (__ 12 3))
(= 3 (__ 5 9))
(= 9 (__ 9 2))
(= 5 (__ 9 12))
