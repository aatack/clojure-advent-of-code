(ns advent-of-code-2023.parsing
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(defn parse-grid
  "Parse a grid, where line breaks in the string represent new rows.
   
   Returns a mapping from coordinates to characters, where the x-coordinate is the
   column and the y-coordinate is the row."
  [characters]
  (into {}
        (for [[row line] (enumerate (split-lines characters))
              [column character] (enumerate line)]
          [[column row] character])))

(defn parse-chunks
  "Parse an input into nested vectors of strings, split at every blank line."
  [input]
  (->> input
       split-lines
       (partition-by #(= 0 (count %)))
       (remove #(= % [""]))))
