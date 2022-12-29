(defn chainable-by-deletion [left right]
  (and (= (count left) (inc (count right)))
       (some (fn [index]
               (= (concat (take index left) (drop (inc index) left))
                  (apply list right)))
             (range (inc (count right))))))

(defn chainable-by-replacement [left right]
  (and (= (count left) (count right))
       (some (fn [index]
          (chainable-by-deletion
           left
           (apply str (concat (take index right)
                              (drop (inc index) right)))))
        (range (inc (count right))))))

(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
(= false (__ #{"cot" "hot" "bat" "fat"}))
(= false (__ #{"to" "top" "stop" "tops" "toss"}))
(= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (__ #{"share" "hares" "shares" "hare" "are"}))
(= false (__ #{"share" "hares" "hare" "are"}))
