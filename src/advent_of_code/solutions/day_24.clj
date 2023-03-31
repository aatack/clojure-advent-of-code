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
        blizzard? (set (keys directions))
        height (count lines)
        width (count (first lines))]
    (reduce (fn [state [x y cell]]
                (cond
                  (and (= y 1) (= cell \.)) (assoc state :start [x y])
                  (and (= y height) (= cell \.)) (assoc state :end [x y])
                  (blizzard? cell) (update-in state [:blizzards [x y]]
                                              #(conj (or % ()) (directions cell)))
                  :else state))
              {:blizzards {}
               :bounds {[1 0] [0 (dec width)]
                        [-1 0] [0 2]
                        [0 1] [1 (dec height)]
                        [0 -1] [1 2]}}
              cells)))

(defn propagate-blizzard [state [position direction]]
  (let [inverse (apply vector (map #(* -1 %) direction))
        [dimension limit] (get-in state [:bounds direction])
        walled? (= (nth position dimension) limit)]
    (if walled?
      [(assoc position dimension (second (get-in state [:bounds inverse])))
       direction]
      [(apply vector (map + position direction))
       direction])))

(defn day-24a [input]
  (let [state (parse-blizzards input)]
    (propagate-blizzard state [[2 6] [0 1]])))

(defn day-24b [input]
  (->> input))
