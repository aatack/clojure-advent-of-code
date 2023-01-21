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

(= true (__ [[:a :b]]))
(= false (__ [[:a :a] [:b :b]]))
(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
              [:a :d] [:b :d] [:c :d]]))
(= true (__ [[1 2] [2 3] [3 4] [4 1]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))
(= false (__ [[1 2] [2 3] [2 4] [2 5]]))
