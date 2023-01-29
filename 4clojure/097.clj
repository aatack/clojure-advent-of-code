(defn __ [row]
  (letfn [(next-row [current-row]
            (map #(apply + %)
                 (partition 2 1 (concat [0] current-row [0]))))]
    (if (= row 1)
      [1]
      (next-row (__ (dec row))))))

(= (__ 1) [1])
(= (map __ (range 1 6))
   [[1]
    [1 1]
    [1 2 1]
    [1 3 3 1]
    [1 4 6 4 1]])
(= (__ 11)
   [1 10 45 120 210 252 210 120 45 10 1])
(= (__ 11)
   [1 10 45 120 210 252 210 120 45 10 1])
