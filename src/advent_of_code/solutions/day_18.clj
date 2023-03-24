(ns advent-of-code.solutions.day-18
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-droplets [input]
  (->> input
       split-lines
       (map #(map read-string (split % #",")))
       (map #(apply vector %))
       set))

(defn day-18a [input]
  (->> input
       parse-droplets))

(defn day-18b [input]
  (->> input))
