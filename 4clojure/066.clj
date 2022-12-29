(def __
  (fn [& numbers]
    (->> (range 1 (apply max numbers))
         (filter (fn [index]
                   (every? (fn [number] (= 0 (mod number index)))
                           numbers)))
         (apply max))))

(= (__ 2 4) 2)
(= (__ 10 5) 5)
(= (__ 5 7) 1)
(= (__ 1023 858) 33)
