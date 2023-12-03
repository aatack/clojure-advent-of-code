(ns advent-of-code-2023.solutions.day-02
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split split-lines]]))

(defn parse-draw [draw]
  (->> (map #(filter seq (split % #" ")) (split draw #","))
       (map (fn [[amount colour]] [(keyword colour) (read-string amount)]))
       (into {})))

(defn parse-game [game]
  (let [[id draws] (split game #":")]
    {:id (-> id (split #" ") second read-string)
     :draws (into [] (map parse-draw (split draws #";")))}))

(defn balls-required [game]
  (reduce (fn [balls [colour amount]]
            (update balls colour #(max amount (or % 0))))
          {}
          (mapcat identity (:draws game))))

(defn possible? [game balls]
  (let [game-balls (balls-required game)]
    (every? (fn [[colour amount]]
              (<= (get game-balls colour 0) amount))
            balls)))

(defn day-02a [input]
  (->> input))

(defn day-02b [input]
  (->> input))

(comment

  (def game "Game 44: 6 green, 1 red; 3 red, 11 green, 2 blue; 2 green, 2 red, 3 blue; 1 red, 15 green, 2 blue")

  (possible? (parse-game game) {:green 15 :red 3 :blue 3}))
