;; NOTE: I realise in hindsight that it would have been much simpler to have
;;       a look for desirable structures within the existing data structure,
;;       rather than bother implementing all the shearing behaviour; I'm too
;;       far down this road now, though...

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
                             (filter #(>= (count (% :minerals)) 3))
                             set)
                   (rotations rock)))

          (total-size [rock]
            (let [minerals (count (rock :minerals))
                  waste (count (rock :waste))
                  size (+ minerals waste)
                  purity (/ minerals size)]
              (* -1 purity)))

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

          (pure? [rock]
            (if (empty? (rock :waste))
              (apply max (map (fn [rotation]
                                (vertical-isosceles-size rotation [0 0]))
                              (rotations rock)))
              0))

          (beam-search [initial-node explore heuristic accept? beam-width]
            (loop [queued-nodes [initial-node]
                   attempts 0]
              (let [node (first queued-nodes)
                    nodes (rest queued-nodes)]
                (cond
                  (nil? node) nil
                  (>= (accept? node) 4) (accept? node)
                  :else (recur (take beam-width
                                     (sort-by heuristic
                                              (concat nodes (explore node))))
                               (inc attempts))))))]

    (beam-search (rotate (rotate (parse-rock ocr-output)))
                 shears
                 total-size
                 pure?
                 1000)
    #_(-> ocr-output
          parse-rock
          rotate
          rotate
          (diagonal-isosceles-size [2 1]))))

(__ [1 3 7 15 31])
(__ [1 2])
(__ [21 10 21 10])
(__ [17 22 6 14 22])
