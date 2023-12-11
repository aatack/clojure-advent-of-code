(ns advent-of-code-2023.solutions.day-10
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-nodes [input]
  (->> input
       parse-grid
       (filter (fn [[_ pipe]] (not= pipe \.)))
       (map (fn [[coordinate pipe]] [coordinate {:pipe pipe}]))
       (into {})))

(defn day-10a [input]
  (->> input
       parse-nodes))

(defn day-10b [input]
  (->> input))
