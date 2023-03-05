(ns advent-of-code.solutions.day-04
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-assignment [assignment]
  (->> (split assignment #",")
       (map #(split % #"-"))
       flatten
       (map read-string)
       (partition 2)))

(defn contains [outer inner]
  (and (<= (first outer) (first inner))
       (>= (second outer) (second inner))))

(defn overlaps [left right]
  (not (or (< (second left) (first right))
           (< (second right) (first left)))))

(defn day-04a [input]
  (->> input
       split-lines
       (map parse-assignment)
       (filter #(or (contains (first %) (second %))
                    (contains (second %) (first %))))
       count))

(defn day-04b [input]
  (->> input
       split-lines
       (map parse-assignment)
       (filter #(apply overlaps %))
       count))
