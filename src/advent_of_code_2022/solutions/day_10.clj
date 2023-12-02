(ns advent-of-code-2022.solutions.day-10
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
       butlast
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
  (let [signal (signal-strength input)]
    (->> (map (fn [index]
                (if (<= 0 (- (mod index 40) (signal index)) 2)
                  "#"
                  "."))
              (sort (keys signal)))
         (partition 40)
         (map #(apply str %)))))
