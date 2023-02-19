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
                  minimum-x (apply min (map first coordinates))
                  minimum-y (apply min (map second coordinates))]
              (transform rock (fn [[x y]] [(- x minimum-x) (- y minimum-y)]))))

          (rotate [rock]
            (focus (transform rock (fn [[x y]] [(- 0 y) x]))))

          (rotations [rock]
            (take 4 (iterate rotate rock)))

          (shear-vertical [rock vertical]
            (focus (transform rock (fn [[x y]] (when (<= x vertical) [x y])))))

          (shear-diagonal [rock diagonal]
            (focus (transform rock (fn [[x y]] (when (<= (+ x y) diagonal) [x y])))))]

    (rotate (parse-rock ocr-output))))

(__ [1 3 7 15 31])
(__ [1 2])
