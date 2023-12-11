(ns advent-of-code-2023.solutions.day-10
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-pipes [input]
  (->> input
       parse-grid
       (filter (fn [[_ pipe]] (not= pipe \.)))
       (map (fn [[coordinate pipe]] [coordinate {:pipe pipe}]))
       (into {})))

(defn starting-coordinates [pipes]
  (->> pipes
       (filter #(= (-> % val :pipe) \S))
       first
       key))

(defn day-10a [input]
  (->> input
       parse-pipes
       starting-coordinates))

(defn day-10b [input]
  (->> input))
