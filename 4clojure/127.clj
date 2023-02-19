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

          (vertical-isosceles-size [rock [x y]]
            (let [dimension
                  (or (last (take-while (fn [size]
                                          (every?
                                           (rock :minerals)
                                           (for [index (range size)]
                                             [(+ x (- size index 1))
                                              (+ y index)])))
                                        (rest (range))))
                      0)]
              (/ (+ (* dimension dimension) dimension) 2)))

          (diagonal-isosceles-size [rock [x y]]
            (let [dimension
                  (or (last (take-while (fn [size]
                                          (every?
                                           (rock :minerals)
                                           (concat (for [index (range size)]
                                                     [(+ x (- size index 1))
                                                      (+ y index)])
                                                   (for [index (range size)]
                                                     [(- x (- size index 1))
                                                      (+ y index)]))))
                                        (rest (range))))
                      0)]
              (* dimension dimension)))

          (isosceles-size [rock point]
            (max (vertical-isosceles-size rock point)
                 (diagonal-isosceles-size rock point)))]

    (let [maximum-size
          (apply max
                 (apply concat
                        (for [rotation (rotations (parse-rock ocr-output))]
                          (map #(isosceles-size rotation %)
                               (rotation :minerals)))))]
      (when (>= maximum-size 3) maximum-size))))

(__ [1 3 7 15 31])
(__ [1 2])
(__ [21 10 21 10])
(__ [17 22 6 14 22])
