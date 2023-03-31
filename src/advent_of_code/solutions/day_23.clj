(ns advent-of-code.solutions.day-23
  (:require [advent-of-code.solutions.day-22 :refer [add]]
            [advent-of-code.utils :refer [enumerate repeat-sequence]]
            [clojure.string :refer [split-lines]]))

(defn directions []
  (repeat-sequence [[0 -1]
                    [0 1]
                    [-1 0]
                    [1 0]]))

(defn parse-positions [input]
  (into #{} (for [[y row] (enumerate (split-lines input))
                  [x cell] (enumerate row)
                  :when (= cell \.)]
              [x y])))

(defn diagonals [direction]
  (for [step [-1 0 1]]
    (apply vector (map #(if (= % 0) step %) direction))))

(defn proposal [state position direction-order]
  (first (for [direction (take 4 direction-order)
               :when (every? #(not (state (add position %))) (diagonals direction))]
           (add position direction))))

(defn day-23a [input]
  (->> input
       parse-positions))

(defn day-23b [input]
  (->> input))
