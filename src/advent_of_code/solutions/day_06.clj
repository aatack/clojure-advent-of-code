(ns advent-of-code.solutions.day-06)

(defn start-of-message-marker [distinct-characters buffer]
  (->> buffer
       (partition distinct-characters 1)
       (map set)
       (map #(= (count %) distinct-characters))
       (take-while not)
       count
       (+ distinct-characters)))

(defn day-06a [input]
  (start-of-message-marker 4 input))

(defn day-06b [input]
  (start-of-message-marker 14 input))
