(require 'clojure.set)

(defn __ [& numbers]
  (letfn [(factors [number]
            (set (filter #(= 0 (rem number %)) (range 1 (inc number)))))

          (greatest-common-denominator [sequence]
            (apply max (reduce clojure.set/intersection
                               (map factors sequence))))

          (numerator' [number]
            (if (integer? number)
              number
              (numerator number)))

          (denominator' [number]
            (if (integer? number)
              1
              (denominator number)))]

    (/ (reduce * (map numerator' numbers))
       (greatest-common-denominator (map denominator' numbers)))))

(== (__ 2 3) 6)
(== (__ 5 3 7) 105)
(== (__ 1/3 2/5) 2)
(== (__ 3/4 1/6) 3/2)
(== (__ 7 5/7 2 3/5) 210)
