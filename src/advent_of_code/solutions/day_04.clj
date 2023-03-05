(ns advent-of-code.solutions.day-04
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-assignment [assignment]
  (->> (split assignment #",")
       (map #(split % #"-"))
       flatten
       (map read-string)
       (partition 2)))

(defn day-04a [input]
  (->> input
       split-lines
       (map parse-assignment)))

(defn day-04b [input]
  (->> input))
