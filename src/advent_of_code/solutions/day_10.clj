(ns advent-of-code.solutions.day-10
  (:require [clojure.string :refer [split-lines split]]))

(defn signal-strength [input]
  (->> input
       split-lines
       (map #(split % #" "))
       (mapcat (fn [command]
                 (if (= command ["noop"])
                   [0]
                   [0 (read-string (second command))])))
       (reductions + 1)
       (zipmap (rest (range)))))

(defn nths [indices sequence]
  (map #(sequence %) indices))

(defn day-10a [input]
  (let [indices [20 60 100 140 180 220]]
    (->> input
         signal-strength
         (nths indices)
         (map * indices)
         (apply +))))

(defn day-10b [input]
  (->> input))
