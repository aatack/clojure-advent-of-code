(def __ (fn [& numbers]
          (if (empty? numbers)
            0
            (reduce
             (fn [left right]
               (if (> left right) left right))
             numbers))))

(= (__ 1 8 3 4) 8)
(= (__ 30 20) 30)
(= (__ 45 67 11) 67)
