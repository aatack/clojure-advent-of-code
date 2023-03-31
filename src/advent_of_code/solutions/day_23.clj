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
  
  (when (> (count (filter state (for [x [-1 0 1]
                                      y [-1 0 1]]
                                  [(+ x (first position)) (+ y (second position))])))
           1)
    (first (for [direction (take 4 priority)
               :when (every? #(not (state (add position %))) (diagonals direction))]
           (add position direction)))))

(defn compute-proposals [state priority]
  (reduce (fn [results position]
            (let [proposal (compute-proposal state position priority)]
              (if proposal
                (update results proposal #(conj (or % ()) position))
                results)))
          {}
          state))

(defn bounds [state]
  (let [xs (map first state)
        ys (map second state)]
    (- (* (inc (- (apply max xs) (apply min xs)))
          (inc (- (apply max ys) (apply min ys))))
       (count state))))

(defn update-state [[state priority]]
  (let [proposals (compute-proposals state priority)
        moves (for [[proposal positions] proposals
                    :when (= (count positions) 1)]
                [(first positions) proposal])]
    [(-> (apply disj state (map first moves))
        (into (map second moves)))
     (rest priority)]))

(defn day-23a [input]
  (->> [(parse-positions input) (directions)]
       (iterate update-state)
       (drop 10)
       ffirst
       bounds))

(defn day-23b [input]
  (->> input))
