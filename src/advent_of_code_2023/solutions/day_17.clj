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
        (remove nil? [(when (< (:times node) 2)
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

(defn minimum-heat-loss [explore]
  (loop [queue #{initial-node (assoc initial-node :direction :down)}
         index {}]
    (if (empty? queue)
      (reverse (sort-by #(vector (apply + (-> % :position))
                                 (* -1 (:loss %))) (mapcat second index)))
      (let [node (first queue)]
        (recur (apply conj (disj queue node)
                      (filter (fn [candidate]
                                (let [existing (index (dissoc candidate :loss))]
                                  (or (empty? existing)
                                      (< (:loss candidate)
                                         (apply min (map :loss existing))))))
                              (explore node)))
               (update index (dissoc node :loss) #(conj (or % []) node)))))))

(defn day-17a [input]
  (:loss (first (minimum-heat-loss (explore-node (parse-blocks input))))))

(defn explore-ultra-node [blocks]
  (let [target [(->> blocks keys (map first) (apply max))
                (->> blocks keys (map second) (apply max))]]
    (fn [node]
      (if (= (:position node) target)
        []
        (remove nil? [(let [position (move-direction (:direction node)
                                                     (:position node))
                            loss (blocks position)]
                        (when (and loss (< (:times node) (dec 10)))
                          (-> node
                              (assoc :position position)
                              (update :times inc)
                              (update :loss + loss)
                              (with-meta {:source node}))))
                      (let [direction (turn-direction :clockwise (:direction node))
                            position (move-direction direction (:position node))
                            loss (blocks position)]
                        (when (and loss (>= (:times node) 3))
                          (-> node
                              (assoc :direction direction)
                              (assoc :position position)
                              (assoc :times 0)
                              (update :loss + loss)
                              (with-meta {:source node}))))
                      (let [direction (turn-direction :anticlockwise (:direction node))
                            position (move-direction direction (:position node))
                            loss (blocks position)]
                        (when (and loss (>= (:times node) 3))
                          (-> node
                              (assoc :direction direction)
                              (assoc :position position)
                              (assoc :times 0)
                              (update :loss + loss)
                              (with-meta {:source node}))))])))))

(defn day-17b [input]
  (->> (minimum-heat-loss (explore-ultra-node (parse-blocks input)))
       (filter #(>= (:times %) 3))
       first
       :loss))
