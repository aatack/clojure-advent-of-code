(defn __ [graph]
  (letfn [(pairs [sequence]
            (map vector (butlast sequence) (rest sequence)))

          (permutations [sequence]
            (if (< (count sequence) 2)
              [sequence]
              ;; TODO: find a better way of excluding duplicates
              (set (apply
                    concat
                    (map (fn [drop-index]
                           (let [head (take drop-index sequence)
                                 tail (drop (inc drop-index) sequence)
                                 pivot (nth sequence drop-index)]
                             (for [permutation (permutations (concat head tail))
                                   insert-index (range (inc (count sequence)))]
                               (concat (take insert-index permutation)
                                       [pivot]
                                       (drop insert-index permutation)))))
                         (range (count sequence)))))))]

    (boolean (some (fn [path]
                     (every? (fn [inputs]
                               (= (last (first inputs)) (first (last inputs))))
                             (pairs path)))
                   (permutations graph)))))
