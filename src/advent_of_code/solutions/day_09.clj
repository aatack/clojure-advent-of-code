(ns advent-of-code.solutions.day-09
  (:require [clojure.string :refer [split-lines split]]))

(defn head-positions [input]
  (->> input
       split-lines
       (mapcat (fn [line]
                 (let [[direction steps] (split line #" ")]
                   (repeat (read-string steps) ({"L" [-1 0]
                                                 "R" [1 0]
                                                 "U" [0 1]
                                                 "D" [0 -1]} direction)))))
       (reductions (fn [position step] (map + position step)) [0 0])))

(defn day-09a [input]
  (head-positions input))

(defn day-09b [input]
  (->> input))
