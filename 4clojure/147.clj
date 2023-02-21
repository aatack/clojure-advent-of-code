(defn __ [initial]
  (iterate (fn [row]
             (let [pairs (map vector row (rest row))]
               (concat [(first row)]
                       (map #(apply +' %) pairs)
                       [(last row)])))
           initial))

(= (second (__ [2 3 2])) [2 5 5 2])
(= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
(= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
(= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
