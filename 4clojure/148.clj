(defn __ [n a b]
  (letfn [(triangle [k] (/ (+ (* k k) k) 2))]
    (let [half-block-size (+ (* a (triangle (dec b)))
                             (* b (triangle (dec a))))
          full-block-size (* (+ 1 (dec a) (dec b))
                             (* a b))
          blocks (quot n (bigint (* a b)))]

      (+ (* half-block-size blocks)
         (* full-block-size (triangle (dec blocks)))
         (apply + (filter #(or (= 0 (mod % a)) (= 0 (mod % b)))
                          (range (* blocks a b) n)))))))

(= 0 (__ 3 17 11))
(= 23 (__ 10 3 5))
(= 233168 (__ 1000 3 5))
(= "2333333316666668" (str (__ 100000000 3 5)))
(= "110389610389889610389610"
   (str (__ (* 10000 10000 10000) 7 11)))
(= "1277732511922987429116"
   (str (__ (* 10000 10000 10000) 757 809)))
(= "4530161696788274281"
   (str (__ (* 10000 10000 1000) 1597 3571)))
