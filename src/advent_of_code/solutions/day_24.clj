(ns advent-of-code.solutions.day-24
  (:require [advent-of-code.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(def directions {\> [1 0]
                 \< [-1 0]
                 \v [0 1]
                 \^ [0 -1]})

(defn parse-blizzards [input]
  (let [lines (split-lines input)
        cells (for [[y row] (enumerate lines 1)
                    [x cell] (enumerate row 1)]
                [x y cell])
        blizzard? (set (keys directions))]
    (reduce (fn [state [x y cell]]
                (cond
                  (and (= y 1) (= cell \.)) (assoc state :start [x y])
                  (and (= y (count lines)) (= cell \.)) (assoc state :end [x y])
                  (blizzard? cell) (update-in state [:blizzards [x y]]
                                              #(conj (or % ()) (directions cell)))
                  :else state))
              {:blizzards {}}
              cells)))

(defn day-24a [input]
  (parse-blizzards input))

(defn day-24b [input]
  (->> input))
