(ns advent-of-code.solutions.day-12
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.utils :refer [breadth-first-search]]))

(def start-height (int \a))
(def end-height (int \z))
(def wall-height 200)

(defn parse-height-map [input]
  (->> input
       split-lines
       (map #(map (fn [character]
                    (cond
                      (= character \S) start-height
                      (= character \E) end-height
                      :else (int character)))
                  %))
       (map #(apply vector %))
       (apply vector)))

(defn height [height-map coordinate]
  ;; Surround the "playable" area with a wall that's too high to
  ;; escape
  (get-in height-map coordinate wall-height))

(defn find-coordinates [height-map value]
  (first (for [row (range (count height-map))
        column (range (count (first height-map)))
        :let [coordinate [row column]]
        :when (= (height height-map coordinate) value)
        ]
    coordinate)))

(defn day-12a [input]
  (let [height-map (parse-height-map input)
        start (find-coordinates height-map start-height)
        end (find-coordinates height-map end-height)
        path (breadth-first-search
              start
              (fn [[x y]]
                (let [maximum-height (inc (height height-map [x y]))]
                  (filter #(<= (height height-map %) maximum-height)
                          [[(inc x) y]
                           [(dec x) y]
                           [x (inc y)]
                           [x (dec y)]])))
              #(= % end))]
    (count path)))

(defn day-12b [input]
  (->> input))
