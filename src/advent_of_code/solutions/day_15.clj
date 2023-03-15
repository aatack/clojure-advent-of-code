(ns advent-of-code.solutions.day-15
  (:require [clojure.string :refer [split-lines replace split]]))

(defn parse-sensors-and-beacons [input]
  (->> (-> input
           (replace #"," "")
           (replace #":" "")
           (replace #"x=" "")
           (replace #"y=" "")
           split-lines)
       (map #(split % #" "))
       (map #(filter integer? (map read-string %)))
       (map (fn [[sensor-x sensor-y beacon-x beacon-y]]
              [[sensor-x sensor-y] [beacon-x beacon-y]]))))

(defn day-15a [input]
  (->> input
       parse-sensors-and-beacons))

(defn day-15b [input]
  (->> input))
