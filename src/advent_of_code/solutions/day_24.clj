(ns advent-of-code.solutions.day-24
  (:require [advent-of-code.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(def directions {\> [1 0]
                 \< [-1 0]
                 \v [0 1]
                 \^ [0 -1]})

(defn parse-state [input]
  (let [lines (split-lines input)
        cells (for [[y row] (enumerate lines 1)
                    [x cell] (enumerate row 1)]
                [x y cell])
        blizzard? (set (keys directions))
        height (count lines)
        width (count (first lines))]
    (reduce (fn [state [x y cell]]
                (cond
                  (and (= y 1) (= cell \.)) (-> state
                                                (assoc :start [x y])
                                                (assoc :agent [x y]))
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

(defn in-bounds? [state [x y]]
  (let [bounds (state :bounds)]
    (and (<= (second (bounds [-1 0])) x (second (bounds [1 0])))
         (<= (second (bounds [0 -1])) y (second (bounds [0 1]))))))

(defn propagate-blizzard [state [position direction]]
  (let [inverse (apply vector (map #(* -1 %) direction))
        [dimension limit] (get-in state [:bounds direction])
        walled? (= (nth position dimension) limit)]
    (if walled?
      [(assoc position dimension (second (get-in state [:bounds inverse])))
       direction]
      [(apply vector (map + position direction))
       direction])))

(defn propagate-state [state]
  (assoc state :blizzards
         (reduce
          (fn [stepped-state blizzard]
            (let [[position direction] (propagate-blizzard state blizzard)]
              (update stepped-state position #(conj (or % ()) direction))))
          {}
          (for [[position blizzards] (state :blizzards)
                direction blizzards]
            [position direction]))))

(defn day-24a [input]
  (let [state (parse-state input)]
    (in-bounds? state [6 6])))

(defn day-24b [input]
  (->> input))
