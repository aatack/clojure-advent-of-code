(defn __ [ocr-output]
  (letfn [(power [number]
            (apply * (repeat number 2)))

          (binary [characters number]
            (first (reduce (fn [[digits remaining] exponent]
                             (if (>= remaining (power exponent))
                               [(conj digits 1) (- remaining (power exponent))]
                               [(conj digits 0) remaining]))
                           [[] number]
                           (reverse (range characters)))))

          (leaves [root value]
            (if (sequential? value)
              (apply concat (map (fn [index item] (leaves (conj root index) item))
                                 (range) value))
              [[root value]]))

          (parse-rock [numbers]
            (let [maximum (apply max numbers)
                  power-of-two (first (drop-while #(>= maximum (power %)) (range)))
                  matrix (map #(binary power-of-two %) numbers)
                  coordinates (leaves [] matrix)]
              {:minerals (set (map first (filter #(= (second %) 1) coordinates)))
               :waste (set (map first (filter #(= (second %) 0) coordinates)))}))

          (transform [rock function]
            {:minerals (set (filter identity (map function (rock :minerals))))
             :waste (set (filter identity (map function (rock :waste))))})

          (focus [rock]
                 ;; Ensure all the defined coordinates have positive x- and y-values
            (let [coordinates (concat (rock :minerals) (rock :waste))
                  minimum-x (if (empty? coordinates)
                              0
                              (apply min (map first coordinates)))
                  minimum-y (if (empty? coordinates)
                              0
                              (apply min (map second coordinates)))]
              (transform rock (fn [[x y]] [(- x minimum-x) (- y minimum-y)]))))

          (rotate [rock]
            (-> rock
                (transform (fn [[x y]] [(- 0 y) x]))
                focus))

          (rotations [rock]
            (take 4 (iterate rotate rock)))

          (shear-vertical [rock vertical]
            (-> rock
                (transform (fn [[x y]] (when (>= x vertical) [x y])))
                focus))

          (shear-diagonal [rock diagonal]
            (-> rock
                (transform (fn [[x y]] (when (>= (+ x y) diagonal) [x y])))
                focus))

          (shears [rock]
            (apply disj (->> (for [rotation (rotations rock)]
                               (concat (take-while #(not (empty? (% :minerals)))
                                                   (map #(shear-vertical rotation %)
                                                        (range)))
                                       (take-while #(not (empty? (% :minerals)))
                                                   (map #(shear-diagonal rotation %)
                                                        (range)))))
                             (apply concat)
                             set)
                   (rotations rock)))

          (size [rock]
                 ;; Give rocks a slightly higher score if they contain less waste
            (let [minerals (count (rock :minerals))
                  waste (count (rock :waste))]
              (* -1 (+ minerals (/ minerals (+ minerals waste 1))))))

          (pure? [rock]
            (empty? (rock :waste)))

          (beam-search [initial-node explore heuristic accept? beam-width]
            (loop [queued-nodes [initial-node]
                   attempts 0]
              (let [node (first queued-nodes)
                    nodes (rest queued-nodes)]
                (if (accept? node)
                  node
                  (recur (take beam-width
                               (sort-by heuristic
                                        (concat nodes (explore node))))
                         (inc attempts))))))]

    (beam-search (parse-rock ocr-output)
        shears
        size
        pure?
        10)
    ;; (parse-rock ocr-output)
    #_(-> ocr-output
        parse-rock
        (shear-vertical 2)
        rotate
        rotate
        (shear-vertical 1)
        rotate
        ;; rotate
        (shear-vertical 1)
        shears)
    #_(shears {:minerals #{[0 1]} :waste #{[0 0]}})))

(__ [1 3 7 15 31])
(__ [1 2])
(__ [21 10 21 10])
