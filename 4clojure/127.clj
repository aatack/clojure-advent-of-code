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
                  minimum-x (apply min 0 (map first coordinates))
                  minimum-y (apply min 0 (map second coordinates))]
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
            (disj (->> (for [rotation (rotations rock)]
                         (concat (take-while #(not (empty? (% :minerals)))
                                             (map #(shear-vertical rotation %)
                                                  (range)))
                                 (take-while #(not (empty? (% :minerals)))
                                             (map #(shear-diagonal rotation %)
                                                  (range)))))
                       (apply concat)
                       set)
                  rock))

          (score [rock]
                 ;; Give rocks a slightly higher score if they contain less waste
            (let [minerals (count (rock :minerals))
                  waste (count (rock :waste))]
              (+ minerals (/ minerals (+ minerals waste)))))

          (accept? [rock]
            (empty? (rock :waste)))]

    (map accept? (shears (parse-rock ocr-output)))))

(__ [1 3 7 15 31])
(__ [1 2])
