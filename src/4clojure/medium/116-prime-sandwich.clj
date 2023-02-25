(defn __ [number]
  (letfn [(prime? [number]
            (case number
              0 false
              1 false
              2 true
              (every? #(not= (mod number %) 0) (range 2 number))))]
    (let [lower-prime (first (filter prime? (range (dec number) 0 -1)))
          upper-prime (first (filter prime? (range (inc number) 1e9)))]
      (and (not= nil lower-prime)
           (not= nil upper-prime)
           (prime? number)
           (= number (/ (+ lower-prime upper-prime) 2))))))

(= false (__ 4))
(= true (__ 563))
(= 1103 (nth (filter __ (range)) 15))
