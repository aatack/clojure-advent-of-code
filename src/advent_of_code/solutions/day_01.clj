(ns advent-of-code.solutions.day-01 
  (:require [advent-of-code.utils :refer [load-input]]
            [clojure.string :refer [split-lines]]))

(defn day-01a [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map read-string %))
       (map #(apply + %))
       (apply max)))

(day-01a (load-input :day-01a))