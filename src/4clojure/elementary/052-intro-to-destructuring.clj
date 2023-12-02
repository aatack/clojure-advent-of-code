(def __ '[c e])

(= [2 4] #_{:clj-kondo/ignore [:unused-binding]}
   (let [[a b c d e f g] (range)] [c e]))
