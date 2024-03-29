(ns advent-of-code-2022.solutions.day-12
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.utils :refer [breadth-first-search]]))

;; NOTE: this may technically make some paths that were previously
;;       possible impossible, since eg. the start will not be able to
;;       hop straight from `S` to `b`
(def start-height (dec (int \a)))
(def end-height (inc (int \z)))
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
               :when (= (height height-map coordinate) value)]
           coordinate)))

(defn shortest-path [height-map start end]
  (let [path (breadth-first-search
              start
              (fn [[x y]]
                (let [maximum-height (inc (height height-map [x y]))]
                  (filter #(<= (height height-map %) maximum-height)
                          [[(inc x) y]
                           [(dec x) y]
                           [x (inc y)]
                           [x (dec y)]])))
              #(= % end))]
    (when path (dec (count path)))))

(defn day-12a [input]
  (let [height-map (parse-height-map input)
        start (find-coordinates height-map start-height)
        end (find-coordinates height-map end-height)]
    (shortest-path height-map start end)))

(defn find-lower-coordinates [height-map value]
  (for [row (range (count height-map))
        column (range (count (first height-map)))
        :let [coordinate [row column]]
        :when (<= (height height-map coordinate) value)]
    coordinate))

(defn day-12b [input]
  (let [height-map (parse-height-map input)
        starts (find-lower-coordinates height-map (int \a))
        end (find-coordinates height-map end-height)]
    (->> starts
         (map #(shortest-path height-map % end))
         (filter identity)
         (apply min))))
