(defn __ [graph]
  (letfn [(drop-first [value sequence]
            (cond
              (empty? sequence) ()
              (= value (first sequence)) (rest sequence)
              :else (cons (first sequence) (drop-first value (rest sequence)))))

          (has-path? [node edges]
            (if (empty? edges)
              true
              (some (fn [edge]
                      (let [next-node (cond
                                        (= node (first edge)) (second edge)
                                        (= node (second edge)) (first edge)
                                        :else nil)]
                        (when next-node (has-path? next-node (drop-first edge edges)))))
                    edges)))]

    (boolean (some #(has-path? % graph) (flatten graph)))))

(= true (__ [[:a :b]]))
(= false (__ [[:a :a] [:b :b]]))
(= false (__ [[:a :b] [:b :a] [:a :c] [:c :a]
              [:a :d] [:b :d] [:c :d]]))
(= true (__ [[1 2] [2 3] [3 4] [4 1]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))
(= false (__ [[1 2] [2 3] [2 4] [2 5]]))
