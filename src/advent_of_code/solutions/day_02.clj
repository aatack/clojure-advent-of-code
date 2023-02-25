(ns advent-of-code.solutions.day-02 
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :refer [split-lines]]))

(def beats {:rock :paper
            :paper :scissors
            :scissors :rock})

(defn points [player opponent]
  (+ ({:rock 1
       :paper 2
       :scissors 3} player)
     (cond
       (= player (beats opponent)) 6
       (= player opponent) 3
       :else 0)))

(defn day-02a [input]
  (->> input
       split-lines
       (map #(points ({\X :rock
                       \Y :paper
                       \Z :scissors} (nth % 2))
                     ({\A :rock
                      \B :paper
                      \C :scissors} (nth % 0))))
       (apply +)))

(defn day-02b [input]
  (->> input
       split-lines
       (map #(let [opponent ({\A :rock
                              \B :paper
                              \C :scissors} (nth % 0))]
               (points (case (nth % 2)
                         \X ((map-invert beats) opponent)
                         \Y opponent
                         \Z (beats opponent))
                       opponent)))
       (apply +)))
