(defn numbers-summing-to [total quantity]
  "Generate all permutations of `quantity` non-negative integers summing
   to a certain total."
  (cond
    (or (= quantity 0) (= total 0)) ()
    (= quantity 1) (list [total])
    :else (for [number (range (inc total))
                others (numbers-summing-to (- total number) (dec quantity))]
            (conj others number))))

(defn variable-length-partition [sequence lengths]
  (when (not-empty lengths)
    (let [quantity (first lengths)]
      (cons (take quantity sequence)
            (variable-length-partition (drop quantity sequence)
                                       (rest lengths))))))

(defn dynamic-for [item-lists]
  ;; TODO: implement this properly
  (if (= (count item-lists) 1)
    (map list (first item-lists))
    (for [head (first item-lists)
          tail (dynamic-for (rest item-lists))]
      (cons head tail))))

(defn possible-expressions
  ([numbers equality]
   (if (= (count numbers) 1)
     [(first numbers)]
     (for [operator (if equality '[=] '[+ - *])
          ;; TODO: also include permutations in the loop
           partitions (map (fn [sequence] (remove #(= % 0) sequence))
                           (numbers-summing-to (count numbers)
                                               (count numbers)))
           :when (> (count partitions) 1)
           :let [groups (variable-length-partition numbers partitions)]
           expressions (dynamic-for (map possible-expressions groups))]
       (cons operator expressions))))
  ([numbers]
   (possible-expressions numbers false)))

(defn __ [& numbers]
  (let [target (last numbers)
        expressions (filter (fn [expression] (= target (eval expression)))
                            (possible-expressions (butlast numbers)))]
    (when (not-empty expressions)
      (let [count (fn [expression] (count (re-seq #"\(" (str expression))))
            expression (apply min-key count expressions)]
        (list '= expression target)))))

(= (__ 3 4 7) '(= (+ 3 4) 7))
(= (__ 3 4 12) '(= (* 3 4) 12))
(= (__ 3 4 14) nil)
(= (__ 3 4 5 35) '(= (* (+ 3 4) 5) 35))
(= (__ 3 4 5 60) '(= (+ (* 3 4) 5) 60)) ; This example is actually faulty
(= (__ 3 4 5 23) '(= (+ 3 (* 4 5)) 23))
(= (__ 3 4 5 27) '(= (* 3 (+ 4 5)) 27))
(= (__ 3 4 5 6) nil)
(= (__ 1 2 10 100 2001) '(= (+ 1 (* 2 10 100)) 2001))
(= (__ 1 2 10 100 1300) '(= (* (+ 1 2 10) 100) 1300))
