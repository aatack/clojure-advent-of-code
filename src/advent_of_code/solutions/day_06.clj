(ns advent-of-code.solutions.day-06)

(defn day-06a [input]
  (->> input
       (partition 4 1)
       (map set)
       (map #(= (count %) 4))
       (take-while not)
       count
       (+ 4)))

(defn day-06b [input]
  (->> input))
