(defn __ [value base]
  (let [result (if (= value 0)
                 [0]
                 (let [digit (rem value base)]
                   (conj (alternative (/ (- value digit) base) base)
                         digit)))]
    ;; Strip the leading zero if there are other digits
    (if (= 1 (count result))
      result
      (rest result))))

(= [1 2 3 4 5 0 1] (__ 1234501 10))
(= [0] (__ 0 11))
(= [1 0 0 1] (__ 9 2))
(= [1 0] (let [n (rand-int 100000)] (__ n n)))
;; Taken from the maximum safe integer in javascript
(= [22 6 10 5 0 19 6 9 6 31] (__ 9007199254740991 42))
