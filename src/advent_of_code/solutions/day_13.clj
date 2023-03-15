(ns advent-of-code.solutions.day-13)(ns advent-of-code.solutions.day-13
  (:require [clojure.string :refer [split-lines]]))

(defn parse-pairs [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(map read-string %))))

(defn compare-lists [left right]
  (cond
    (= left right)
    nil

    (and (integer? left) (integer? right))
    (< left right)
    
    (integer? left)
    (compare-lists [left] right)
    
    (integer? right)
    (compare-lists left [right])
    
    :else
    (let [comparison (reduce
                      (fn [_ [inner-left inner-right]]
                        (let [comparison (compare-lists inner-left inner-right)]
                          (if (nil? comparison)
                            nil
                            (reduced comparison))))
                      nil
                      (map vector left right))]
      (if (nil? comparison)
        (compare-lists (count left) (count right))
        comparison))))

(defn day-13a [input]
  (->> input
       parse-pairs
       (map vector (rest (range)))
       (filter #(apply compare-lists (second %)))
       (map first)
       (apply +)))

(defn day-13b [input]
  (let [left-divider [[2]]
        right-divider [[6]]
        packets (->> input
                     parse-pairs
                     (mapcat identity)
                     (concat [left-divider right-divider])
                     (sort compare-lists)
                     (apply vector))]
    (* (inc (.indexOf packets left-divider))
       (inc (.indexOf packets right-divider)))))
