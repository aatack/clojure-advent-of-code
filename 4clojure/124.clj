(defn __ [board player]
  (letfn [(in-bounds? [[x y]]
            (and (<= 0 x 3) (<= 0 y 3)))

          (piece-sequence [coordinate direction]
            (let [delta ({:x+ [1 0] :x- [-1 0] :y+ [0 1] :y- [0 -1]} direction)]
              (if (in-bounds? coordinate)
                (cons [coordinate (get-in board coordinate)]
                      (piece-sequence (map + coordinate delta) direction))
                [])))

          (flipped [coordinate]
            (for [direction [:x+ :x- :y+ :y-]
                  [piece-coordinate piece]
                  (take-while #(not (#{'e player} (second %)))
                              (rest (piece-sequence coordinate direction)))]
              piece-coordinate))]

    (apply hash-map (apply concat (filter #(not (empty? (second %)))
                                          (for [x (range 0 4)
                                                y (range 0 4)
                                                :when (= (get-in board [x y]) 'e)]
                                            [[x y] (set (flipped [x y]))]))))))

(__ '[[e e e e]
      [e w b e]
      [e b w e]
      [e e e e]] 'w)
