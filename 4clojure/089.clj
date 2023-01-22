(defn __ [graph]
  (letfn [(has-path [node edges backwards]
            (if (empty? edges)
              true
              (some (fn [edge]
                      (has-path (if backwards (first edge) (second edge))
                                (disj edges edge)
                                false))
                    (filter #(= ((if backwards first second) %) node) edges))))]

    (let [edges (set graph)]
      (boolean (some #(or (has-path % edges true) (has-path % edges false)) (set (flatten graph)))))))

(defn has-path? [node edges]
  (if (empty? edges)
    true
    (some (fn [edge]
            (let [next-node (cond
                              (= node (first edge)) (second edge)
                              (= node (second edge)) (first edge)
                              :else nil)]
              (when next-node (has-path? next-node (disj edges edge)))))
          edges)))


(has-path? 3 #{[1 2]})


(= true (__ [[:a :b]]))
(= false (__ [[:a :a] [:b :b]]))
(= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
              [:a :d] [:b :d] [:c :d]]))
(= true (__ [[1 2] [2 3] [3 4] [4 1]]))
(= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
             [:b :e] [:a :d] [:b :d] [:c :e]
             [:d :e] [:c :f] [:d :f]]))
(= false (__ [[1 2] [2 3] [2 4] [2 5]]))

(__  [[:a :b] [:a :c] [:c :b]])

;; --- Something like "some (fn [edge] (and (has node edge))) edges"
