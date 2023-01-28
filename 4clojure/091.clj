(defn __ [graph]
  (loop [nodes #{(ffirst (vec graph))}
         edges graph]
    (if (empty? nodes)
      (boolean (empty? graph))
      (let [new-edges (filter
                       (fn [edge] (or (nodes (first edge)) (nodes (second edge))))
                       edges)]
        (recur (set (flatten new-edges))
               (apply disj edges new-edges))))))

(= true (__ #{[:a :a]}))
(= true (__ #{[:a :b]}))
(= false (__ #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]}))
(= true (__ #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]}))
(= false (__ #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]}))
(= true (__ #{[:a :b] [:b :c] [:c :d]
              [:x :y] [:d :a] [:b :e] [:x :a]}))
