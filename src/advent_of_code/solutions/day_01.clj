(ns advent-of-code.solutions.day-01 
  (:require [advent-of-code.utils :refer [defsolution]]
            [clojure.string :refer [split-lines]]))

(defsolution day-01a
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map read-string %))
       (map #(apply + %))
       (apply max)))

(day-01a)

