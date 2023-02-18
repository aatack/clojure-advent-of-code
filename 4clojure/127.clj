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
                  power-of-two (first (drop-while #(> maximum (power %)) (range)))
                  matrix (map #(binary power-of-two %) numbers)
                  coordinates (leaves [] matrix)]
              {:minerals (set (map first (filter #(= (second %) 1) coordinates)))
               :waste (set (map first (filter #(= (second %) 0) coordinates)))}))]

    (parse-rock ocr-output)))

(__ [1 3 7 15 31])
