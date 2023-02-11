(defn __ [function sequence]
  (if (empty? sequence)
    ()
    (lazy-seq (cons (function (first sequence))
                    (__ function (rest sequence))))))

(= [3 4 5 6 7]
   (__ inc [2 3 4 5 6]))
(= (repeat 10 nil)
   (__ (fn [_] nil) (range 10)))
(= [1000000 1000001]
   (->> (__ inc (range))
        (drop (dec 1000000))
        (take 2)))
(= [1000000 1000001]
   (->> (__ inc (range))
        (drop (dec 1000000))
        (take 2)))