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
  (map #(nth sequence %) indices))

(defn day-10a [input]
  (let [indices [20 60 100 140 180 220]]
    (->> input
         signal-strength
         ;; By adding a dud element to the front, we conveniently
         ;; transition to a 1-based indexing system like the one
         ;; used in the prompt
         (cons 0)
         (apply vector)
         (nths indices)
         (map * indices)
         (apply +))))

(defn day-10b [input]
  (->> input))
