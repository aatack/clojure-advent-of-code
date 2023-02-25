(defn custom-count
 ([sequence] (custom-count sequence 0))
 ([sequence acc]
  (if (empty? sequence)
    acc
    (recur (rest sequence) (inc acc)))))

(def __ custom-count)

(= (__ '(1 2 3 3 1)) 5)
(= (__ "Hello World") 11)
(= (__ [[1 2] [3 4] [5 6]]) 3)
(= (__ '(13)) 1)
(= (__ '(:a :b :c)) 3)
