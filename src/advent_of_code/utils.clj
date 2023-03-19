(ns advent-of-code.utils
  (:require [clojure.string :as string]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))

(defn transpose [sequences]
  (apply map vector sequences))

(defn breadth-first-search [initial explore accept?]
  (loop [queue [initial]
         path {initial :start}]
    (let [node (first queue)]
      (cond
        (nil? node) nil
        (accept? node) (butlast (take-while identity (iterate path node)))
        :else (let [nodes (filter (complement path) (explore node))]
                (recur (concat (rest queue) nodes)
                       (reduce #(assoc %1 %2 node) path nodes)))))))

(defn beam-search
  [initial-node explore evaluate space limit]
  (loop [queued-nodes [[(evaluate initial-node) initial-node]]
         attempts 0
         seen-nodes (set queued-nodes)]
    (if (or (> attempts limit) (empty? queued-nodes))
      (apply max-key first seen-nodes)
      (let [[score node] (first queued-nodes)
            nodes (rest queued-nodes)
            explored (apply disj (set (map #(vector (evaluate %) %)
                                           (explore node))) seen-nodes)]
        (recur (take space (reverse (sort-by first
                                             (concat nodes explored))))
               (inc attempts)
               (apply conj seen-nodes explored))))))

(defn graph-distance
  "Compute the shortest distance from one node to another in a graph.
   
   The graph should be a map from a node to the set of other nodes that
   are reachable from it.  No information should be assigned to the
   edges in this setup."
  [graph start end]
  (-> (breadth-first-search start
                            graph
                            #(= % end))
      count
      dec))

(defn inclusive-range [start end]
  (range (min start end) (inc (max start end))))

(defn merge-ranges
  "Merge two ranges, described by pairs of inclusive integers."
  [left right]
  (let [[[left-start left-end] [right-start right-end]]
        (sort-by first [left right])]
    (if (> right-start (inc left-end))
      [[left-start left-end] [right-start right-end]]
      [nil [left-start (max left-end right-end)]])))

(defn conj-range
  "Add a new range to an ordered collection of ranges."
  [collection addition]
  (let [[body tail]
        (reduce (fn [[acc old-range] new-range]
                  (let [[left right] (merge-ranges old-range
                                                   new-range)]
                    [(if left (conj acc left) acc)
                     right]))
                [[] addition]
                collection)]
    (conj body tail)))

(defn count-ranges [ranges]
  (->> ranges
       (map (fn [[left right]] (inc (- right left))))
       (apply +)))

(count-ranges [[1 2] [5 10]])
