(ns advent-of-code.solutions.day-15
  (:require [clojure.string :refer [split-lines replace split]]
            [advent-of-code.utils :refer [conj-range]]))

(defn parse-sensors-and-beacons [input]
  (->> (-> input
           (replace #"," "")
           (replace #":" "")
           (replace #"x=" "")
           (replace #"y=" "")
           split-lines)
       (map #(split % #" "))
       (map #(filter integer? (map read-string %)))))

(defn exclusion-range [[sensor-x sensor-y beacon-x beacon-y] y]
  (let [distance (+ (abs (- sensor-x beacon-x))
                    (abs (- sensor-y beacon-y)))
        width (- distance (abs (- sensor-y y)))]
    (when (>= width 0)
      [(- sensor-x width) (+ sensor-x width)])))

(defn day-15a [input]
  (->> input
       parse-sensors-and-beacons
       (map #(exclusion-range % 10))))

(defn day-15b [input]
  (->> input))
