(defn custom-range [start end]
  (if (= start end)
    '()
    (cons start (custom-range (inc start) end))))

(def __ custom-range)

(= (__ 1 4) '(1 2 3))
(= (__ -2 2) '(-2 -1 0 1))
(= (__ 5 8) '(5 6 7))
