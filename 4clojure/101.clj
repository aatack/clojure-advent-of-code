(defn __ [start target]
  (let [start' (map identity start)
        target' (map identity target)
        characters (set target')]
    (letfn [(beam-search [initial-node explore evaluate space]
              (loop [queued-nodes [initial-node]
                     attempts 0]
                (let [node (first queued-nodes)
                      nodes (rest queued-nodes)]
                  (if (= 0 (evaluate node))
                    attempts
                    (recur (take space (sort-by evaluate
                                                (concat nodes
                                                        (explore node))))
                           (inc attempts))))))

            (delete [index word]
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
              (+
               (abs (- (count word) (count target')))
               (if (empty? word) 0
                   (/ (reduce + (map #(if (= %1 %2) 0 1) word target'))
                      (count word)))))]

      (beam-search start' explore evaluate 10))))

(= (__ "kitten" "sitting") 3)
(= (__ "closure" "clojure") (__ "clojure" "closure") 1)
(= (__ "xyx" "xyyyx") 2)
(= (__ "" "123456") 6)
(= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)
(= (__ [1 2 3 4] [0 2 3 4 5]) 2)
(= (__ '(:a :b :c :d) '(:a :d)) 2)
(= (__ "ttttattttctg" "tcaaccctaccat") 10)
(= (__ "gaattctaatctc" "caaacaaaaaattt") 9)
