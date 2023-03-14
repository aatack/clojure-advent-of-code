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
      (if (accept? node)
        (butlast (take-while identity (iterate path node)))
        (let [nodes (filter (complement path) (explore node))]
          (recur (concat (rest queue) nodes)
                 (reduce #(assoc %1 %2 node) path nodes)))))))
