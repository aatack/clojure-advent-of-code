(ns advent-of-code-2023.solutions.day-08
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

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

(defn ghost-steps-required [nodes mapping instructions steps]
  (if (every? ending-node? nodes)
    steps
    (recur (doall (map #(get-in mapping [% (first instructions)]) nodes))
           mapping
           (rest instructions)
           (inc steps))))

(defn cycle-length
  "Determine the cycle length of a specific starting node.
   
   For some reason it looks like the nodes always have a constant cycle length, with no
   offset from the zeroth step."
  ([node mapping instructions]
   (cycle-length node mapping instructions 0 {}))
  ([node mapping instructions step visits]
   (let [identifier [node (first instructions)]]
     (if (ending-node? node)
       step
       (recur (get-in mapping [node (second (first instructions))])
              mapping
              (rest instructions)
              (inc step)
              (assoc visits identifier step))))))

(defn day-08b [input]
  (let [{:keys [instructions nodes]} (parse-input input)
        starting-nodes (filter starting-node? (keys nodes))]
    (->> starting-nodes
         (map #(cycle-length % nodes (cycle (enumerate instructions))))
         (apply *'))))
