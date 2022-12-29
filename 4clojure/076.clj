(def __ [1 3 5 7 9 11])

(= __
   (letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]
     (trampoline foo [] 1)))
