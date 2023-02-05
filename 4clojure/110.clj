(defn __ [initial-sequence]
  (letfn [(pronounce [sequence]
            (->> sequence
                 (partition-by identity)
                 (map #(list (count %) (first %)))
                 (apply concat)))]
    (pronounce initial-sequence)))

(__ [1 1 2])
