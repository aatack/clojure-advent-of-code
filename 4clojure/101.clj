(defn naive-levenshtein [left right]
  (if (= left right)
    0
    (letfn [(delete [index sequence]
              (concat (take index sequence)
                      (nthrest sequence (inc index))))
            (insert [index character sequence]
              (concat (take index sequence) [character] (nthrest sequence index)))
            (modify [index character sequence]
              (concat (take index sequence)
                      [character]
                      (nthrest sequence (inc index))))]
      (let [sequences (concat (map #(delete % left) (range (count left)))
                              (for [index (range (inc (count left)))
                                    character (set right)]
                                (insert index character left))
                              (for [index (range (count left))
                                    character (set right)]
                                (modify index character left)))]
        (inc (apply min (count right)
                    (map #(naive-levenshtein % right) sequences)))))))
