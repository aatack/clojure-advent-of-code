(ns advent-of-code-2023.solutions.day-03
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn digit? [character]
  (.contains "0123456789" (str character)))

(defn parse-schematic [input]
  (let [grid (filter (fn [[_ character]] (not= character \.)) (parse-grid input))]
    {:digits (into {} (filter (fn [[_ character]] (digit? character)) grid))
     :symbols (into {} (filter (fn [[_ character]] (not (digit? character))) grid))}))

(defn flag-part-digits [schematic]
  (map (fn [[[column row] digit]]
         [[column row]
          [digit
           (some (:symbols schematic)
                 (for [x [(dec column) column (inc column)]
                       y [(dec row) row (inc row)]]
                   [x y]))]])
       (:digits schematic)))

(defn day-03a [input]
  (->> input
       parse-schematic
       flag-part-digits))

(defn day-03b [input]
  (->> input))
