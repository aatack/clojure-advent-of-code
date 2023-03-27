(ns advent-of-code.solutions.day-20
  (:require [clojure.string :refer [split-lines]]))

(defn parse-input [input]
  (let [lines (split-lines input)
        limit (dec (count lines))]
    (->> lines
         (map-indexed (fn [index value]
                        [index {:value (read-string value)
                                :left (if (> index 0)
                                        (dec index)
                                        limit)
                                :right (if (< index limit)
                                         (inc index)
                                         0)}]))
         (into {}))))

(defn peek [file value distance]
  (let [direction (if (< distance 0) :left :right)]
    (loop [current-value value
           remaining-distance (abs distance)]
      (if (= remaining-distance 0)
        current-value
        (recur (file (current-value direction))
               (dec remaining-distance))))))

(defn day-20a [input]
  (let [file (parse-input input)]
    (peek file (file 0) -37080)))

(defn day-20b [input]
  (->> input))
