(ns advent-of-code-2023.solutions.day-16
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

(defn move [direction [x y]]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))

(defn propagate [mirror direction]
  (case mirror
    \. [direction]
    \\ (case direction
         :right [:down]
         :down [:right]
         :up [:left]
         :left [:up])
    \/ (case direction
         :right [:up]
         :down [:left]
         :up [:right]
         :left [:down])
    \| (if (#{:left :right} direction)
         [:up :down]
         [direction])
    \- (if (#{:up :down} direction)
         [:left :right]
         [direction])
    nil []))

(defn explore [children initial]
  (loop [unexplored #{initial}
         explored #{}]
    (if (empty? unexplored)
      explored
      (let [node (first unexplored)
            new-nodes (->> (children node) (remove explored) (remove unexplored))]
        (println unexplored)
        (recur (into #{} (apply conj (rest unexplored) new-nodes))
               (conj explored node))))))

(defn day-16a [input]
  (->> input))

(defn day-16b [input]
  (->> input))
