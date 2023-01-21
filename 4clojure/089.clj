(defn __ [graph]
  (letfn [(has-path [first-edge remaining-edges]
            (if (empty? remaining-edges)
              true
              (some (fn [second-edge]
                      (has-path second-edge (disj remaining-edges second-edge)))
                    (filter #(= (first %) (second first-edge))
                            remaining-edges))))]

    (let [edges (set graph)]
      (boolean (some #(has-path % (disj edges %)) edges)))))

(= true (__ [[:a :b]]))
(= false (__ [[:a :a] [:b :b]]))
(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
              [:a :d] [:b :d] [:c :d]]))
(= true (__ [[1 2] [2 3] [3 4] [4 1]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))
(= false (__ [[1 2] [2 3] [2 4] [2 5]]))
