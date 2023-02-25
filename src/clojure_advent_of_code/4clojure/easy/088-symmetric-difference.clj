(defn __ [left right]
  (let [xor (fn [a b] (and (or a b) (not (and a b))))]
    (set (filter (fn [value] (xor (left value) (right value)))
                 (concat left right)))))

(= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
(= (__ #{:a :b :c} #{}) #{:a :b :c})
(= (__ #{} #{4 5 6}) #{4 5 6})
(= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
