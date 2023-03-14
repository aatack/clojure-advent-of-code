(ns advent-of-code.solutions.day-13)(ns advent-of-code.solutions.day-13
  (:require [clojure.string :refer [split-lines]]))

(defn parse-pairs [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map read-string %))
       first
       ))

(defn day-13a [input]
  (parse-pairs input))

(defn day-13b [input]
  (->> input))
