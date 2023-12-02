(def __ 7)

(= __ (let [x 5] (+ 2 x)))
(= __ (let [x 3, y 10] (- y x)))
(= __ (let [x 21]
        #_{:clj-kondo/ignore [:redundant-let]}
        (let [y 3] (/ x y))))
