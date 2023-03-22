(ns advent-of-code.solutions.day-17
  (:require [advent-of-code.utils :refer [repeat-sequence]]))

(defn parse-jets [input]
  (->> input
       (map {\> [1 0] \< [-1 0]})
       repeat-sequence))

(defn parse-pieces []
  (repeat-sequence [#{[0 0] [1 0] [2 0] [3 0]}
      #{[0 1] [1 1] [2 1] [1 2] [1 0]}
      #{[0 0] [1 0] [2 0] [2 1] [2 2]}
      #{[0 0] [0 1] [0 2] [0 3]}
      #{[0 0] [1 0] [0 1] [1 1]}]))

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

(defn step [pieces jets terrain]
  (let [piece (first pieces)
        jetted-piece (move piece (first jets))
        moved-piece (if (collides? jetted-piece terrain)
                      piece
                      jetted-piece)
        dropped-piece (move moved-piece [0 -1])]
    (if (collides? dropped-piece terrain)
      ;; Piece has settled
      (let [new-terrain (apply conj terrain moved-piece)
            remaining-pieces (rest pieces)]
        (lazy-seq (cons new-terrain
                        (step (cons (initialise (first remaining-pieces) new-terrain)
                                    (rest remaining-pieces))
                              (rest jets)
                              new-terrain))))
      ;; Piece has dropped
      (step (cons dropped-piece (rest pieces))
            (rest jets)
            terrain))))

(defn day-17a [input]
  (let [terrain #{}
        pieces (parse-pieces)
        steps (step (cons (initialise (first pieces) terrain)
                          (rest pieces))
                    (parse-jets input)
                    terrain)]
    (-> steps
        (nth (dec 2022))
        height)))

(defn day-17b [input]
  (->> input))
