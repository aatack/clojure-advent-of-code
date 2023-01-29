(defn __ [row]
  (letfn [(next-row [current-row]
            (map #(apply + %)
                 (partition 2 1 (concat [0] current-row [0]))))]
    (if (= row 1)
      [1]
      (next-row (__ (dec row))))))
