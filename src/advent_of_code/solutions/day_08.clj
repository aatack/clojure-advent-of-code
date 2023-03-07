(ns advent-of-code.solutions.day-08
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.utils :refer [transpose]]))

(defn parse-forest [input]
  (->> input
       split-lines
       (map #(map str %))
       (map #(map read-string %))))

(defn visible-from-front 
  "Get the coordinates of trees in a row visible from the front."
  [trees]
  (first (reduce (fn [[coordinates tallest] [coordinate height]]
                   (if (= height 9)
                     (reduced [(conj coordinates coordinate) height])
                     (if (> height tallest)
                       [(conj coordinates coordinate) height]
                       [coordinates tallest])))
                 [[] -1]
                 (map vector (range) trees))))

(defn visible-from-back
  "Get the coordinates of trees in a row visible from the back."
  [trees]
  (let [length (count trees)]
    (map #(- length % 1) (visible-from-front (reverse trees)))))

(defn day-08a [input]
  (let [forest (->> input parse-forest)]
    (-> (concat (mapcat (fn [index row]
                          (map (fn [coordinate] [index coordinate])
                               (concat (visible-from-front row)
                                       (visible-from-back row))))
                        (range) forest)
                (mapcat (fn [index column]
                          (map (fn [coordinate] [coordinate index])
                               (concat (visible-from-front column)
                                       (visible-from-back column))))
                        (range) (transpose forest)))
        set
        count)))

(defn day-08b [input]
  (->> input))
