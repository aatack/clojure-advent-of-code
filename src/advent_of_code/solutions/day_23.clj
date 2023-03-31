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

(defn compute-proposal [state position priority]
  (first (for [direction (take 4 priority)
               :when (every? #(not (state (add position %))) (diagonals direction))]
           (add position direction))))

(defn compute-proposals [state priority]
  (reduce (fn [results position]
            (let [proposal (compute-proposal state position priority)]
              (if proposal
                (update results proposal #(conj (or % ()) position))
                results)))
          {}
          state))

(defn update-state [[state priority]]
  (let [proposals (compute-proposals state priority)
        moves (for [[proposal positions] proposals
                    :when (= (count positions) 1)]
                [(first positions) proposal])]
    (-> (apply disj state (map first moves))
        (into (map second moves)))))

(defn day-23a [input]
  (update-state [(parse-positions input) (directions)]))

(defn day-23b [input]
  (->> input))
