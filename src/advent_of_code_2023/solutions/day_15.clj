(ns advent-of-code-2023.solutions.day-15
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn string-hash [characters]
  (reduce (fn [value character]
            (mod (* 17 (+ value (int character))) 256))
          0
          characters))

(defn day-15a [input]
  (->> input
       (split-string ",")
       (map string-hash)
       (apply +)))

(defn day-15b [input]
  (->> input))
