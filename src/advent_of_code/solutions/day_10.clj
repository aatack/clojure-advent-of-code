(ns advent-of-code.solutions.day-10
  (:require [clojure.string :refer [split-lines split]]))

(defn signal-strength [input]
  (->> input
       split-lines
       (map #(split % #" "))
       (mapcat (fn [command]
                 (if (= command ["noop"])
                   [0]
                   [(read-string (second command)) 0])))
       (reductions +)))

(defn day-10a [input]
  (signal-strength input))

(defn day-10b [input]
  (->> input))
