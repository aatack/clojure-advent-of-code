(ns advent-of-code-2023.solutions.day-21
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [advent-of-code-2023.utils :refer [move-direction]]
            [clojure.string :refer [split-lines]]))

(defn parse-plot [input]
  (let [plot (parse-grid input)]
    {:unreachable (->> plot
                       (filter #(#{\. \S} (second %)))
                       keys
                       (into #{}))
     :reachable (->> plot
                     (filter #(= (second %) \S))
                     keys
                     (into #{}))}))

(defn north-east-south-west [position]
  (map #(move-direction % position) [:up :right :down :left]))

(defn step [plot]
  (assoc plot
         :reachable
         (->> plot
              :reachable
              (mapcat north-east-south-west)
              (filter (:unreachable plot))
              set)))

(defn day-21a [input]
  (->> input
       parse-plot
       (iterate step)
       (drop 64)
       first
       :reachable
       count))

(defn day-21b [input]
  (->> input))
