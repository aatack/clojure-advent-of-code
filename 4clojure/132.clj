(defn __ [predicate value collection]
  (let [interleaves (concat
                     (map #(if (predicate %1 %2)
                             value
                             nil)
                          collection (rest collection))
                     [nil])]
    (filter identity (apply concat (map vector collection interleaves)))))

(= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
(= '(2) (__ > :more [2]))
(= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
(empty? (__ > :more ()))
(= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
   (take 12 (->> [0 1]
                 (iterate (fn [[a b]] [b (+ a b)]))
                 (map first)
                 (__ (fn [a b]
                       (= (even? a) (even? b)))
                     :same))))
