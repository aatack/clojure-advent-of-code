(defn __ [number]
  (let [digits (map {\0 0
                     \1 1
                     \2 2
                     \3 3
                     \4 4
                     \5 5
                     \6 6
                     \7 7
                     \8 8
                     \9 9} (str number))
        length (int (/ (count digits) 2))]
    (boolean (= (apply + (take length digits))
                (apply + (take length (reverse digits)))))))
