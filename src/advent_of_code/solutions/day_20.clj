(ns advent-of-code.solutions.day-20
  (:require [clojure.string :refer [split-lines]]))

(defn parse-input [input]
  (let [lines (split-lines input)
        limit (dec (count lines))]
    (->> lines
        (map-indexed (fn [index value]
                       [index {:value (read-string value)
                               :left (if (> index 0)
                                       (dec index)
                                       limit)
                               :right (if (< index limit)
                                        (inc index)
                                        0)}]))
        (into {}))))

(defn day-20a [input]
  (parse-input input))

(defn day-20b [input]
  (->> input))
