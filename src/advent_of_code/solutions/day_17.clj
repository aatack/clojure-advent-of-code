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

(defn step [pieces jets terrain]
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
            new-jet (first new-jets)
            
            restart? (and (= 0 ((meta new-piece) :index))
                          (= 0 ((meta new-jet) :index)))]
        
        (lazy-seq (cons (with-meta new-terrain {:restart? restart?})
                        (step (cons (initialise (first new-pieces) new-terrain)
                                    (rest new-pieces))
                              new-jets
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
