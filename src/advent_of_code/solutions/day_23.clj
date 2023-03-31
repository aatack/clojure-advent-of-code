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
                  :when (not= cell \.)]
              [x y])))

(defn diagonals [direction]
  (for [step [-1 0 1]]
    (apply vector (map #(if (= % 0) step %) direction))))

(defn proposal [state position priority]
  (first (for [direction (take 4 priority)
               :when (every? #(not (state (add position %))) (diagonals direction))]
           (add position direction))))

(defn proposals [state priority]
  (reduce (fn [results position]
            (let [position-proposal (proposal state position priority)]
              (if position-proposal
                (update results position-proposal #(conj (or % ()) position))
                results)))
          {}
          state))

(defn day-23a [input]
  (proposals (parse-positions input) (directions)))

(defn day-23b [input]
  (->> input))
