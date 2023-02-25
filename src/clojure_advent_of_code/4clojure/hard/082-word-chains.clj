(defn has-valid-word-chain [words]
  (letfn [(chainable-by-deletion [left right]
            (and (= (count left) (inc (count right)))
                 (some (fn [index]
                         (= (concat (take index left) (drop (inc index) left))
                            (apply list right)))
                       (range (inc (count right))))))

          (chainable-by-replacement [left right]
            (and (= (count left) (count right))
                 (some (fn [index]
                         (= [(take index left) (drop (inc index) left)]
                            [(take index right) (drop (inc index) right)]))
                       (range (inc (count right))))))

          (chainable [left right]
            (or (chainable-by-deletion left right)
                (chainable-by-deletion right left)
                (chainable-by-replacement left right)))

          (pairs [sequence]
            (map vector (butlast sequence) (rest sequence)))

          (chain-chainable [chain]
            (every? #(chainable (first %) (second %))
                    (pairs chain)))

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
    (boolean (some chain-chainable
                   (permutations (apply vector words))))))

(def __ has-valid-word-chain)

(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
(= false (__ #{"cot" "hot" "bat" "fat"}))
(= false (__ #{"to" "top" "stop" "tops" "toss"}))
(= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (__ #{"share" "hares" "shares" "hare" "are"}))
(= false (__ #{"share" "hares" "hare" "are"}))
