(ns advent-of-code.solutions.day-08
  (:require [clojure.string :refer [split-lines]]
            [advent-of-code.utils :refer [transpose]]))

(def maximum-height 9)

(defn parse-forest [input]
  (->> input
       split-lines
       (map #(map str %))
       (map #(map read-string %))
       (map #(apply vector %))
       (apply vector)
       ))

(defn visible-from-front 
  "Get the coordinates of trees in a row visible from the front."
  [trees]
  (first (reduce (fn [[coordinates tallest] [coordinate height]]
                   (if (= height maximum-height)
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

(defn view-distance [forest view-height step coordinate]
  (let [tree-height (get-in forest coordinate)]
    (cond
      (nil? tree-height) 0
      (>= tree-height view-height) 1
      :else (+ 1 (view-distance forest
                                view-height
                                step
                                (map + coordinate step))))))

(defn scenic-score [forest coordinate]
  (let [height (get-in forest coordinate)]
    (apply * (map (fn [step]
           (view-distance forest
                          height
                          step
                          (map + coordinate step)))
         [[0 1] [0 -1] [1 0] [-1 0]]))))

(defn day-08b [input]
  (let [forest (->> input parse-forest)]
    nil))
