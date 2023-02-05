(defn __ [initial-sequence]
  (letfn [(pronounce [sequence]
            (->> sequence
                 (partition-by identity)
                 (map #(list (count %) (first %)))
                 (apply concat)))]
    (lazy-seq (cons initial-sequence (__ (pronounce initial-sequence))))))
