(def __
  (fn [initial-edges]
    (letfn [(create-edge [graph [source destination]]
              (update graph
                      source
                      (fn [source']
                        (conj (or source' #{}) destination))))

            (create-edges [graph edges]
              (reduce create-edge graph edges))

            (inbound-nodes [graph destination]
              (for [[source destinations] graph
                    :when (destinations destination)]
                source))

            (outbound-nodes [graph source]
              (or (graph source) ()))

            (nodes [graph]
              (-> (for [[_ edges] graph] (into [] edges))
                  flatten
                  set))

            (edge-exists [graph source destination]
              ((get graph source #{}) destination))

            (missing-edges [graph]
              (for [node (nodes graph)
                    inbound (inbound-nodes graph node)
                    outbound (outbound-nodes graph node)
                    :when (not= inbound outbound)
                    :when (not (edge-exists graph inbound outbound))]
                [inbound outbound]))

            (all-edges [graph]
              (set (for [[source destinations] graph
                         destination destinations]
                     [source destination])))]

      (let [graph (create-edges {} initial-edges)]
        (all-edges (loop [graph' graph]
                     (let [missing-edges' (missing-edges graph')]
                       (if (empty? missing-edges')
                         graph'
                         (recur (create-edges graph' missing-edges'))))))))))

(let [divides #{[8 4] [9 3] [4 2] [27 9]}]
  (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
(let [more-legs
      #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
  (= (__ more-legs)
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
(let [progeny
      #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
  (= (__ progeny)
     #{["father" "son"] ["father" "grandson"]
       ["uncle" "cousin"] ["son" "grandson"]}))
