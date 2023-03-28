(ns advent-of-code.solutions.day-22
  (:require [clojure.string :refer [split-lines]]))

(defn parse-maze [input]
  "")

(defn parse-instructions [input]
  (->> input
       (partition-by #{\L \R})
       (map (fn [instruction]
              (cond
                (= (first instruction) \L) [:turn :left]
                (= (first instruction) \R) [:turn :right]
                :else [:move (read-string (apply str instruction))])))))

(defn parse-input [input]
  (let [[maze _ [instructions]] (->> input split-lines (partition-by empty?))]
    [(parse-maze maze) (parse-instructions instructions)]))

(defn day-22a [input]
  (parse-input input))

(defn day-22b [input]
  (->> input))

(partition-by #{\L \R} "ABCLDHG")
