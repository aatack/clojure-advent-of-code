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

    (/ (reduce * numbers)
       (greatest-common-denominator (map denominator' numbers)))))


