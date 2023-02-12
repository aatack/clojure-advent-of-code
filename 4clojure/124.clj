(defn __ [board player]
  (letfn [(in-bounds? [[x y]]
            (and (<= 0 x 3) (<= 0 y 3)))

          (piece-sequence [coordinate direction]
            (let [delta ({:x+ [1 0] :x- [-1 0] :y+ [0 1] :y- [0 -1]} direction)]
              (if (in-bounds? coordinate)
                (cons [coordinate (get-in board coordinate)]
                      (piece-sequence (map + coordinate delta) direction))
                [])))

          (sequence-flips [sequence]
            (reduce (fn [flips [piece-coordinate piece]]
                      (cond
                        (= piece 'e) (reduced [])
                        (= piece player) (reduced flips)
                        :else (conj flips [piece-coordinate piece])))
                    []
                    (concat (rest sequence) [[nil 'e]])))

          (flipped [coordinate]
            (for [direction [:x+ :x- :y+ :y-]
                  [piece-coordinate piece]
                  (sequence-flips (piece-sequence coordinate direction))]
              piece-coordinate))]

    (apply hash-map (apply concat (filter #(not (empty? (second %)))
                                            (for [x (range 0 4)
                                                  y (range 0 4)
                                                  :when (= (get-in board [x y]) 'e)]
                                              [[x y] (set (flipped [x y]))]))))))

(= {[1 3] #{[1 2]}
    [0 2] #{[1 2]}
    [3 1] #{[2 1]}
    [2 0] #{[2 1]}}
   (__ '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))
(= {[3 2] #{[2 2]}
    [3 0] #{[2 1]}
    [1 0] #{[1 1]}}
   (__ '[[e e e e]
         [e w b e]
         [w w w e]
         [e e e e]] 'b))
(= {[0 3] #{[1 2]}
    [1 3] #{[1 2]}
    [3 3] #{[2 2]}
    [2 3] #{[2 2]}}
   (__ '[[e e e e]
         [e w b e]
         [w w b e]
         [e e b e]] 'w))
(= {[0 3] #{[2 1] [1 2]}
    [1 3] #{[1 2]}
    [2 3] #{[2 1] [2 2]}}
   (__ '[[e e w e]
         [b b w e]
         [b w w e]
         [b w w w]] 'b))
(= {[0 3] #{[2 1] [1 2]}
    [1 3] #{[1 2]}
    [2 3] #{[2 1] [2 2]}}
   (__ '[[e e w e]
         [b b w e]
         [b w w e]
         [b w w w]] 'b))
