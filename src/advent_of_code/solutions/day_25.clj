(ns advent-of-code.solutions.day-25
  (:require [advent-of-code.utils :refer [power]]))

(def snafu-characters {\= -2
                       \- -1
                       \0 0
                       \1 1
                       \2 2})

(defn snafu->decimal [string]
  (apply +' (map *'
                 (map snafu-characters (reverse string))
                 (map #(power 5 %) (range)))))

(snafu->decimal "1121-1110-1=0")

(defn day-25a [input]
  (->> input))

(defn day-25b [input]
  (->> input))
