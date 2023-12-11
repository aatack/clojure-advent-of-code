(ns advent-of-code-2023.solutions.day-08
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

(defn parse-instructions [characters]
  (->> characters
       (map {\L :left \R :right})))

(defn parse-node [characters]
  [(subs characters 0 3) {:left (subs characters 7 10)
                          :right (subs characters 12 15)}])

(defn parse-input [input]
  (let [lines (split-lines input)]
    {:instructions (parse-instructions (first lines))
     :nodes (into {} (map parse-node (drop 2 lines)))}))

(defn steps-required [node nodes instructions steps]
  (if (= node "ZZZ")
    steps
    (recur (get-in nodes [node (first instructions)])
           nodes
           (rest instructions)
           (inc steps))))

(defn day-08a [input]
  (let [{:keys [instructions nodes]} (parse-input input)]
    (steps-required "AAA" nodes (cycle instructions) 0)))

(defn starting-node? [node]
  (= (last node) \A))

(defn ending-node? [node]
  (= (last node) \Z))

(defn ghost-steps-required [nodes mapping instructions reset steps]
  (if (every? ending-node? nodes)
    steps
    (recur (doall (map #(get-in mapping [% (first instructions)]) nodes))
           mapping
           (if (= (count instructions) 1)
             (doall reset)
             (doall (rest instructions)))
           reset
           (inc steps))))

(defn day-08b [input]
  (let [{:keys [instructions nodes]} (parse-input input)]
    (ghost-steps-required (doall (filter starting-node? (keys nodes)))
                          nodes
                          (doall instructions)
                          (doall instructions)
                          0)))
