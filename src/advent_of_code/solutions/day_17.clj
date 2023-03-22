(ns advent-of-code.solutions.day-17
  (:require [clojure.string :refer [split-lines]]))

(def pieces 
  [#{[0 0] [1 0] [2 0] [3 0]}
   #{[0 0] [1 0] [2 0] [1 1] [1 -1]}
   #{[0 0] [1 0] [2 0] [2 1] [2 2]}
   #{[0 0] [0 1] [0 2] [0 3]}
   #{[0 0] [1 0] [0 1] [1 1]}])

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
  (some terrain piece))

(defn day-17a [input]
  (->> input))

(defn day-17b [input]
  (->> input))
