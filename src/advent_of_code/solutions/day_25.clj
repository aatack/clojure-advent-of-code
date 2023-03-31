(ns advent-of-code.solutions.day-25
  (:require [advent-of-code.utils :refer [power]]
            [clojure.string :refer [split-lines]]))

(def snafu-characters {\= -2
                       \- -1
                       \0 0
                       \1 1
                       \2 2})

(defn snafu->decimal [string]
  (apply + (map *
                (map snafu-characters (reverse string))
                (map #(power 5 %) (range)))))

(defn snafu-digits
  "Work out how many digits a number will have when represented in SNAFU."
  [number]
  (loop [digits 0]
    (if (>= (snafu->decimal (repeat digits \2)) number)
      digits
      (recur (inc digits)))))

(defn decimal->snafu [number]
  (loop [digits (apply vector (repeat (snafu-digits number) \0))
         place 0]
    (if (>= place (count digits))
      (apply str digits)
      (let [options (map #(assoc digits place %) (keys snafu-characters))]
        (recur (apply min-key #(abs (- number (snafu->decimal %))) options)
               (inc place))))))

(defn day-25a [input]
  (->> input
       split-lines
       (map snafu->decimal)
       (apply +)
       decimal->snafu))

(defn day-25b [input]
  (->> input))
