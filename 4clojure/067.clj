(def __
  (fn [n]
    (let [prime? (fn [number]
                   (case number
                     1 false
                     2 true
                     (every? #(not= (mod number %) 0) (range 2 number))))]
      (take n (filter prime? (rest (range)))))))

(= (__ 2) [2 3])
(= (__ 5) [2 3 5 7 11])
(= (last (__ 100)) 541)
