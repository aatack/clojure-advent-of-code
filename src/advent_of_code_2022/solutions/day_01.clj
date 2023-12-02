(ns advent-of-code-2022.solutions.day-01
  (:require [clojure.string :refer [split-lines]]))

(defn calories [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map read-string %))
       (map #(apply + %))))

(defn day-01a [input]
  (->> input calories (apply max)))

(defn day-01b [input]
  (->> input
       calories
       sort
       reverse
       (take 3)
       (apply +)))
