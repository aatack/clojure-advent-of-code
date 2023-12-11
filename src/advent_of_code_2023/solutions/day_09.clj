(ns advent-of-code-2023.solutions.day-09
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-numbers]]
            [clojure.string :refer [split-lines]]))

(defn next-number [numbers]
  (if (= #{0} (set numbers))
    0
    (+ (last numbers)
       (next-number (->> numbers
                         (partition 2 1)
                         (map #(apply - (reverse %))))))))

(defn day-09a [input]
  (->> input
       split-lines
       (map parse-numbers)
       (map next-number)))

(defn day-09b [input]
  (->> input))
