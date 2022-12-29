(require 'clojure.set)

(def __
  (fn [number]
    (max 1 (let [divisors (fn [x] (set (filter #(= (mod x %) 0)
                                               (range 2 (inc x)))))
                 number-divisors (divisors number)
                 coprime? (fn [x] (empty?
                                   (clojure.set/intersection
                                    number-divisors
                                    (divisors x))))]
             (count (filter coprime? (range 1 number)))))))

(= (__ 1) 1)
(= (__ 10) (count '(1 3 7 9)) 4)
(= (__ 40) 16)
(= (__ 99) 60)
