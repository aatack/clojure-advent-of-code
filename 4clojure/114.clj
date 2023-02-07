(defn __ [total predicate sequence]
  (let [head (first sequence)
        decrement (predicate head)]
    (if (and (= total 1) decrement)
      ()
      (lazy-seq (cons head
                      (__ (if decrement (dec total) total)
                          predicate
                          (rest sequence)))))))

(= [2 3 5 7 11 13]
   (__ 4 #(= 2 (mod % 3))
       [2 3 5 7 11 13 17 19 23]))
(= ["this" "is" "a" "sentence"]
   (__ 3 #(some #{\i} %)
       ["this" "is" "a" "sentence" "i" "wrote"]))
(= ["this" "is"]
   (__ 1 #{"a"}
       ["this" "is" "a" "sentence" "i" "wrote"]))
