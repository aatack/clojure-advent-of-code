(ns advent-of-code-2023.solutions.day-17
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [beam-search]]
            [advent-of-code-2023.parsing :refer [parse-grid]]
            [advent-of-code-2023.utils :refer [move-direction turn-direction]]
            [clojure.string :refer [split-lines]]))

(defn parse-blocks [input]
  (->> input
       parse-grid
       (map (fn [[key value]] [key (read-string (str value))]))
       (into {})))

(def initial-node {:position [0 0]
                   :direction :right
                   :times 0
                   :loss 0})

(defn explore-node [blocks]
  (let [target [(->> blocks keys (map first) (apply max))
                (->> blocks keys (map second) (apply max))]]
    (fn [node]
      (if (= (:position node) target)
        []
        (remove nil? [(when (< (:times node) 3)
                        (let [position (move-direction (:direction node)
                                                       (:position node))
                              loss (blocks position)]
                          (when loss
                            (-> node
                                (assoc :position position)
                                (update :times inc)
                                (update :loss + loss)))))
                      (let [direction (turn-direction :clockwise (:direction node))
                            position (move-direction direction (:position node))
                            loss (blocks position)]
                        (when loss
                          (-> node
                              (assoc :direction direction)
                              (assoc :position position)
                              (assoc :times 0)
                              (update :loss + loss))))
                      (let [direction (turn-direction :anticlockwise (:direction node))
                            position (move-direction direction (:position node))
                            loss (blocks position)]
                        (when loss
                          (-> node
                              (assoc :direction direction)
                              (assoc :position position)
                              (assoc :times 0)
                              (update :loss + loss))))])))))

(defn minimum-heat-loss [blocks explore]
  (loop [queue #{initial-node (assoc initial-node :direction :down)}
         index {}]
    (if (or (empty? queue) #_(> (count index) 100))
      index
      (let [node (first queue)]
        (recur (apply conj (disj queue node) (remove #(index (dissoc % :loss)) (explore node)))
               (assoc index (dissoc node :loss) node))))))

(defn evaluate-node [blocks]
  (let [column (->> blocks keys (map first) (apply max))
        row (->> blocks keys (map second) (apply max))]
    (fn [{:keys [position loss]}]
      (let [distance (+ (abs (- (first position) column))
                        (abs (- (second position) row)))]
        (+ (* -1 distance) (* -0.1 loss))))))

(defn day-17a [input]
  (let [blocks (parse-blocks input)
        explore (explore-node blocks)
        evaluate (evaluate-node blocks)]
    (minimum-heat-loss blocks explore)))

(defn day-17b [input]
  (->> input))
