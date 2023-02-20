(defn __ [value base]
  (letfn [(power [exponent]
            (apply * (repeat exponent base)))]

    (loop [remaining value
           digit-index 0
           digit-buffer ()]
      (if (= 0 remaining)
        (if (empty? digit-buffer) '(0) digit-buffer)
        (let [difference (rem remaining (power (inc digit-index)))]
          (recur (- remaining difference)
                 (inc digit-index)
                 (cons (/ difference (power digit-index)) digit-buffer)))))))

(= [1 2 3 4 5 0 1] (__ 1234501 10))
(= [0] (__ 0 11))
(= [1 0 0 1] (__ 9 2))
(= [1 0] (let [n (rand-int 100000)] (__ n n)))
;; Taken from the maximum safe integer in javascript
(= [22 6 10 5 0 19 6 9 6 31] (__ 9007199254740991 42))
