(defn __ [& sequences]
  ;; If each of the sequences start with the same value, we have found the
  ;; smallest number contained in all of them.  Otherwise, throw away the
  ;; first value from the smallest of the sequences
  (let [heads (map first sequences)
        tails (map rest sequences)
        smallest (first (apply min-key second (map-indexed vector heads)))]
    (if (apply = heads)
      (first heads)
      (apply __ (map-indexed
                 (fn [index [head tail]]
                   (if (= index smallest)
                     tail
                     (cons head tail)))
                 (map vector heads tails))))))

(= 3 (__ [3 4 5]))
(= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
(= 64 (__ (map #(* % % %) (range))
          (filter #(zero? (bit-and % (dec %))) (range))
          (iterate inc 20)))
(= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
