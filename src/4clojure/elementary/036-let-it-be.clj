(def __ '[x 7 y 3 z 1])

(= 10 #_{:clj-kondo/ignore [:unused-binding]}
   (let [x 7 y 3 z 1] (+ x y)))
(= 4 #_{:clj-kondo/ignore [:unused-binding]}
   (let [x 7 y 3 z 1] (+ y z)))
(= 1 #_{:clj-kondo/ignore [:unused-binding]}
   (let [x 7 y 3 z 1] z))
