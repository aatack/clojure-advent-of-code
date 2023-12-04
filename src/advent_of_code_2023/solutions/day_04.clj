(ns advent-of-code-2023.solutions.day-04
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [power]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [split-lines]]))

(defn parse-card [card]
  (let [[id numbers] (split-string ":" card)
        [winning drawn] (split-string "\\|" numbers)]
    {:id (->> id (split-string " ") second read-string)
     :drawn (->> drawn (split-string " ") (map read-string) (into #{}))
     :winning (->> winning (split-string " ") (map read-string) (into #{}))}))

(defn points [card]
  (let [winning (intersection (:winning card) (:drawn card))]
    (if (seq winning)
      (->> winning count dec (power 2))
      0)))

(defn day-04a [input]
  (->> input
       split-lines
       (map parse-card)))

(defn day-04b [input]
  (->> input))
