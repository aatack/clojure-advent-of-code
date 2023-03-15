(ns advent-of-code.solutions.day-14
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-rock [coordinates]
  (for [[[start-x start-y] [end-x end-y]] (partition 2 1 coordinates)
        x (range start-x (inc end-x))
        y (range start-y (inc end-y))]
    [x y]))

(defn parse-cave [input]
  (let [coordinates
        (->> input
             split-lines
             (map #(split % #" -> "))
             (map #(map (fn [string]
                          (read-string (str "[" string "]")))
                        %))
             (mapcat parse-rock)
             set)]
    {:coordinates coordinates
     :height (apply max (map second coordinates))}))

(defn day-14a [input]
  (parse-cave input))

(defn day-14b [input]
  (->> input))

(partition 2 1 [1 2 3 4 5])
