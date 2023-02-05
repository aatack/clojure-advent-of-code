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
