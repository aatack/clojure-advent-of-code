(defn __ [n a b]
  (letfn [(triangle [k] (/ (+ (* k k) k) 2))]
    (let [half-block-size (+ (* a (triangle (dec b)))
                             (* b (triangle (dec a))))
          full-block-size (* (+ 1 (dec a) (dec b))
                             (* a b))
          blocks (int (/ n (* a b)))]

      (+ (* half-block-size blocks)
         (* full-block-size (triangle (dec blocks)))
         (apply + (filter #(or (= 0 (mod % a)) (= 0 (mod % b)))
                          (range (* blocks a b) n)))))))

(= 0 (__ 3 17 11))
(= 23 (__ 10 3 5))
(= 233168 (__ 1000 3 5))
(= "2333333316666668" (str (__ 100000000 3 5)))
