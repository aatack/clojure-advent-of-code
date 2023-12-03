(ns advent-of-code-2023.solutions.day-02
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split split-lines]]))

(defn parse-draw [draw]
  (->> (map #(filter seq (split % #" ")) (split draw #","))
       (map (fn [[amount colour]] [colour (read-string amount)]))
       (into {})))

(defn parse-game [game]
  (let [[id draws] (split game #":")]
    {:id (-> id (split #" ") second read-string)
     :draws (map parse-draw (split draws #";"))}))

(defn day-02a [input]
  (->> input))

(defn day-02b [input]
  (->> input))

(comment

  (def game "Game 44: 6 green, 1 red; 3 red, 11 green, 2 blue; 2 green, 2 red, 3 blue; 1 red, 15 green, 2 blue")

  (parse-game game))
