(ns advent-of-code-2023.solutions.day-17
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [advent-of-code-2023.utils :refer [move-direction turn-direction]]
            [clojure.string :refer [split-lines]]))

(defn parse-blocks [input]
  (->> input
       parse-grid
       (map (fn [[key value]] [key (read-string (str value))]))
       (into {})))

(defn explore [blocks]
  (fn [node]
    (remove nil? [(when (< (:times node) 3)
                    (let [position (move-direction (:direction node) (:position node))]
                      (-> node
                          (assoc :position position)
                          (update :times inc)
                          (update :loss + (or (blocks position) 10000)))))
                  (let [direction (turn-direction :clockwise (:direction node))
                        position (move-direction direction (:position node))]
                    (-> node
                        (assoc :direction direction)
                        (assoc :position position)
                        (assoc :times 0)
                        (update :loss + (or (blocks position) 10000))))
                  (let [direction (turn-direction :clockwise (:direction node))
                        position (move-direction direction (:position node))]
                    (-> node
                        (assoc :direction direction)
                        (assoc :position position)
                        (assoc :times 0)
                        (update :loss + (or (blocks position) 10000))))])))

(defn day-17a [input]
  (let [explore-node (->> input
                          parse-blocks
                          explore)]
    (-> {:position [0 0]
                   :direction :right
                   :times 0
                   :loss 0}
        explore-node
        first
        explore-node
        first
        explore-node
        first
        explore-node
        first)))

(defn day-17b [input]
  (->> input))
