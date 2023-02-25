(def __
  (fn [sequence]
    (->> sequence
         (reduce
          (fn [[current best] value]
            (let [longer (fn [left right]
                           (if (> (count left) (count right))
                             left
                             right))
                  new (conj current value)]
              (if (or (empty? current) (< (last current) value))
                [new (longer new best)]
                [[value] (longer [value] best)])))
          [[] []])
         second
         ((fn [result]
            (if (= (count result) 1)
              []
              result))))))

(= (__ [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (__ [5 6 1 3 2 7]) [5 6])
(= (__ [2 3 3 4 5]) [3 4 5])
(= (__ [7 6 5 4]) [])
