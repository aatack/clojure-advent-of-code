(ns advent-of-code.solutions.day-12
  (:require [clojure.string :refer [split-lines]]))

(defn parse-height-map [input]
  (->> input
       split-lines
       (map #(map (fn [character]
                    (cond
                      ;; Ensure we can always step down from the start
                      (= character \S) 100
                      ;; Ensure we can never step down to the end
                      (= character \E) -1
                      :else (- (int character) (int \a))))
                  %))
       (map #(apply vector %))
       (apply vector)))

(defn height [height-map coordinate]
  ;; Surround the "playable" area with a wall that's too high to
  ;; escape
  (get-in height-map coordinate 200))

(defn starting-coordinates [height-map]
  (first (for [row (range (count height-map))
        column (range (count (first height-map)))
        :let [coordinate [row column]]
        :when (= (height height-map coordinate) 100)
        ]
    coordinate)))

(defn day-12a [input]
  (->> input
       parse-height-map
       starting-coordinates))

(defn day-12b [input]
  (->> input))
