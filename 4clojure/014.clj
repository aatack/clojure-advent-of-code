(def __ 8)

(= __ ((fn add-five [x] (+ x 5)) 3))
(= __ ((fn [x] (+ x 5)) 3))
(= __ (#(+ % 5) 3))
(= __ ((partial + 5) 3))
