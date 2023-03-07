(ns advent-of-code.solutions.day-08
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.utils :refer [transpose]]))

(defn parse-forest [input]
  (->> input
       split-lines
       (map #(map str %))
       (map #(map read-string %))))

(defn day-08a [input]
  (->> input))

(defn day-08b [input]
  (->> input))
