(ns advent-of-code.solutions.day-17
  (:require [advent-of-code.utils :refer [repeat-sequence]]))

(defn assign-indices
  "Add an `:index` metadata tag to each of a sequence of items."
  [items]
  (map-indexed (fn [index item] (with-meta item {:index index}))
               items))

(defn parse-jets [input]
  (->> input
       (map {\> [1 0] \< [-1 0]})
       assign-indices
       repeat-sequence))

(defn parse-pieces []
  (-> [#{[0 0] [1 0] [2 0] [3 0]}
       #{[0 1] [1 1] [2 1] [1 2] [1 0]}
       #{[0 0] [1 0] [2 0] [2 1] [2 2]}
       #{[0 0] [0 1] [0 2] [0 3]}
       #{[0 0] [1 0] [0 1] [1 1]}]
      assign-indices
      repeat-sequence))

(defn move
  "Move a piece by the specified step."
  [piece [dx dy]]
  (map (fn [[x y]] [(+ x dx) (+ y dy)]) piece))

(defn collides?
  "Determine whether a piece collides with the terrain.
   
   Terrain should be provided as a function, or function-like
   value, which maps `[x y]` coordinates to `nil` if that
   coordinate is empty, or the coordinate otherwise."
  [piece terrain]
  (letfn [(wall [[x y]]
            (or (< y 0)
                (< x 0)
                (>= x 7)))]
    (or (some wall piece)
        (some terrain piece))))

(defn height [terrain]
  (if (empty? terrain)
    0
    (inc (apply max (map second terrain)))))

(defn initialise [piece terrain]
  (move piece [2 (+ 3 (height terrain))]))

(defn step [pieces jets terrain index]
  (let [piece (first pieces)
        jetted-piece (move piece (first jets))
        moved-piece (if (collides? jetted-piece terrain)
                      piece
                      jetted-piece)
        dropped-piece (move moved-piece [0 -1])]
    (if (collides? dropped-piece terrain)
      ;; Piece has settled
      (let [new-pieces (rest pieces)
            new-terrain (apply conj terrain moved-piece)
            new-jets (rest jets)

            new-piece (first new-pieces)
            new-jet (first new-jets)]

        (lazy-seq (cons (with-meta new-terrain
                          {:index index
                           :piece ((meta new-piece) :index)
                           :jet ((meta new-jet) :index)})
                        (step (cons (initialise (first new-pieces)
                                                new-terrain)
                                    (rest new-pieces))
                              new-jets
                              new-terrain
                              (inc index)))))
      ;; Piece has dropped
      (step (cons dropped-piece (rest pieces))
            (rest jets)
            terrain
            index))))

(defn day-17a [input]
  (let [terrain #{}
        pieces (parse-pieces)
        steps (step (cons (initialise (first pieces) terrain)
                          (rest pieces))
                    (parse-jets input)
                    terrain
                    1)]
    (->> steps
         (drop (dec (+ 15 (* 7 35))))
         (take 10)
         (map meta))))

(defn diff [sequence]
  (->> sequence
       (partition 2 1)
       (map (fn [[left right]] (- right left)))))

(defn find-repeats
  "Find repeating cycles within a sequence.
   
   This will return five values:
   
   - The index (1-indexed) of the first terrain in the repeated
     sequence
   - The index of the first terrain in the next iteration of the
     repeated sequence
   - A map from index to height for each of the terrain instances
     seen, up to and including the latter index
   
   To find a repeating sequence, every time a new terrain is
   yielded, the current piece and jet index are inspected.  If
   the same combination has appeared before, the number of
   terrain instances that have passed since the last one is
   checked.  If this value remains constant for a certain number
   of repeats of the sequence (2), the index of the first and
   second terrain instances in that repeated sequence is returned.

   It is necessary to check for multiple repeats, and to ensure
   they have the same period, because the initial state of the
   flat floor can cause some sequences to be aperiodic."
  [terrain-sequence]
  (loop [indices {}
         history {}
         sequence terrain-sequence]
    (let [terrain (first sequence)

          piece ((meta terrain) :piece)
          jet ((meta terrain) :jet)
          index ((meta terrain) :index)

          key [piece jet]

          updated-indices (update indices
                                  key
                                  #(conj (or % []) index))
          updated-history (assoc history index (height terrain))

          these-indices (updated-indices key)]

      (if (and (> (count these-indices) 2)
               (apply = (diff these-indices)))
        [(first these-indices)
         (second these-indices)
         history]
        (recur updated-indices
               updated-history
               (rest sequence))))))

(defn day-17b [input]
  (let [terrain #{}
        pieces (parse-pieces)
        steps (step (cons (initialise (first pieces) terrain)
                          (rest pieces))
                    (parse-jets input)
                    terrain
                    1)

        ; [[first-index first-height]
        ;  [second-index second-height]
        ;  height-history]
        ; (find-repeats steps)

        ; index-delta (- second-index first-index)
        ; height-delta (- second-height first-height)

        ; total-steps 1736
        ; total-loops (quot (-' total-steps first-index) index-delta)
        ; leftover-steps (-' total-steps
        ;                    first-index
        ;                    (*' total-loops index-delta))
        ]

    (find-repeats steps)
    #_(let [indices (reduce (fn [history terrain]
                              (update history [((meta terrain) :piece) ((meta terrain) :jet)]
                                      #(conj (or % []) ((meta terrain) :index))))
                            {} (take 4000 steps))]
        (map identity (filter
                       #_#(= (first %) 25)
                       #_#(> (count %) 2)
                       #(and (> (count %) 2) (apply not= (diffs %)))
                       (vals indices))))
    ; [first-index first-height leftover-steps total-loops index-delta]
    #_(+' first-height
          (-' (height-history (+' first-index leftover-steps))
              first-height)
          (*' (quot (-' total-steps first-index) index-delta)
              height-delta))))
