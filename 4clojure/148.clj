(defn __ [n a b]
  (letfn [(triangle [k] (/ (+ (* k k) k) 2))]
    (let [half-block-size (+ (* a (triangle (dec b)))
                             (* b (triangle (dec a))))
          full-block-size (* (+ 1 (dec a) (dec b))
                             (* a b))
          blocks (int (/ n (* a b)))]
      
      (+ (* half-block-size blocks)
         (* full-block-size (triangle (dec blocks)))))))
