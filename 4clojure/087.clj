(def __
  (fn [number]
    (letfn [(parse [n]
              ;; TODO: find an easier way of parsing integers
              ({\0 0 \1 1 \2 2 \3 3 \4 4
                \5 5 \6 6 \7 7 \8 8 \9 9} n))

            (happy [n]
              (as-> n $
                (str $)
                (map parse $)
                (map #(* % %) $)
                (apply + $)))

            (terminates? [sequence]
              (reduce (fn [seen value]
                        (cond
                          (= value 1) (reduced true)
                          (seen value) (reduced false)
                          :else (conj seen value)))
                      #{}
                      sequence))]

      (terminates? (iterate happy number)))))

(= (__ 7) true)
(= (__ 986543210) true)
(= (__ 2) false)
(= (__ 3) false)
