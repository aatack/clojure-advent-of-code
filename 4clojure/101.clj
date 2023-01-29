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

(defn beam-search [initial-node explore evaluate space]
  (loop [queued-nodes [initial-node]]
    (let [node (first queued-nodes)
          nodes (rest queued-nodes)]
      (if (= 0 (evaluate node))
        node
        (recur (take space (sort-by evaluate
                                    (concat nodes
                                            (explore node)))))))))

(defn levenshtein-distance [start target]
  (let [characters (set target)]
    (letfn [(delete [index word]
              (concat (take index word)
                      (nthrest word (inc index))))
            (insert [index character word]
              (concat (take index word) [character] (nthrest word index)))
            (modify [index character word]
              (concat (take index word)
                      [character]
                      (nthrest word (inc index))))

            (explore [word]
              (concat (map #(delete % word) (range (count word)))
                      (for [index (range (inc (count word)))
                            character characters]
                        (insert index character word))
                      (for [index (range (count word))
                            character characters]
                        (modify index character word))))
            (evaluate [word]
              (if (not= (count word) (count target))
                (abs (- (count word) (count target)))
                (/ (reduce + (map #(if (= %1 %2) 0 1) word target))
                   (count word))))]

      (evaluate start))))

(levenshtein-distance (map identity "hello") (map identity "hello"))
