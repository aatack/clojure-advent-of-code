(ns advent-of-code-2023.solutions.day-08
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

(defn parse-instructions [characters]
  (->> characters
       (map {\L :left \R :right})
       cycle))

(defn parse-node [characters]
  [(subs characters 0 3) {:left (subs characters 7 10)
                          :right (subs characters 12 15)}])

(defn parse-input [input]
  (let [lines (split-lines input)]
    {:instructions (parse-instructions (first lines))
     :nodes (into {} (map parse-node (drop 2 lines)))}))

(defn day-08a [input]
  (->> input
       parse-input))

(defn day-08b [input]
  (->> input))
