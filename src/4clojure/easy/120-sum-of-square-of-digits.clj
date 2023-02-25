(defn __ [numbers]
  (letfn [(digits [number]
            (map {\0 0
                  \1 1
                  \2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9} (str number)))

          (sum-of-squares [number]
            (apply + (map #(* % %) (digits number))))]

    (->> numbers
         (filter #(< % (sum-of-squares %)))
         count)))

(= 8 (__ (range 10)))
(= 19 (__ (range 30)))
(= 50 (__ (range 100)))
(= 50 (__ (range 1000)))
