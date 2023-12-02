(ns advent-of-code-2022.solutions.day-14
  (:require [clojure.string :refer [split-lines split]]
            [advent-of-code.utils :refer [inclusive-range]]))

(def drop-point [500 0])

(defn parse-rock [coordinates]
  (for [[[start-x start-y] [end-x end-y]] (partition 2 1 coordinates)
        x (inclusive-range start-x end-x)
        y (inclusive-range start-y end-y)]
    [x y]))

(defn parse-cave [floor input]
  (let [coordinates
        (->> input
             split-lines
             (map #(split % #" -> "))
             (map #(map (fn [string]
                          (read-string (str "[" string "]")))
                        %))
             (mapcat parse-rock)
             set)
        depth (apply max (map second coordinates))]
    {:coordinates coordinates
     :depth depth
     :floor (when floor (dec (+ depth floor)))}))

(defn resting-place [cave [x y]]
  (cond
    (and (cave :floor) (>= y (cave :floor)))
    [x y]

    ;; At the same depth as the lowest item, the sand falls
    (and (not (cave :floor)) (>= y (cave :depth))) nil

    (not ((cave :coordinates) [x (inc y)]))
    (resting-place cave [x (inc y)])

    (not ((cave :coordinates) [(dec x) (inc y)]))
    (resting-place cave [(dec x) (inc y)])

    (not ((cave :coordinates) [(inc x) (inc y)]))
    (resting-place cave [(inc x) (inc y)])

    :else [x y]))

(defn drop-sand [cave]
  (let [coordinate (resting-place cave drop-point)]
    (when coordinate
      (update cave :coordinates #(conj % coordinate)))))

(defn day-14a [input]
  (->> input
       (parse-cave nil)
       (iterate drop-sand)
       (take-while identity)
       count
       dec))

(defn day-14b [input]
  (->> input
       (parse-cave 2)
       (iterate drop-sand)
       (take-while #(not ((% :coordinates) drop-point)))
       count))
