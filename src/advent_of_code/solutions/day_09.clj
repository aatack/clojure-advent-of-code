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

(defn clamp [value] (if (< value -1) -1 (if (> value 1) 1 value)))

(defn tail-positions [heads]
  (rest (reductions (fn [tail head]
                      (let [difference (map - head tail)
                            adjacent? (every? #(= (clamp %) %) difference)]
                        (if adjacent?
                          tail
                          (map + tail (map clamp difference)))))
                    [0 0]
                    heads)))

(defn day-09a [input]
  (-> input head-positions tail-positions set count))

(defn day-09b [input]
  (->> input
       head-positions
       (iterate tail-positions)
       (drop 9)
       first
       set
       count))
