(def __
  (fn [number]
    (let [divisors (set (filter #(= (mod number %) 0)
                                (range 1 number)))]
      (= number (apply + divisors)))))

(= (__ 6) true)
(= (__ 7) false)
(= (__ 496) true)
(= (__ 500) false)
(= (__ 8128) true)
