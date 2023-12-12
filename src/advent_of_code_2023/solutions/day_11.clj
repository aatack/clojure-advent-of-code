(ns advent-of-code-2023.solutions.day-11
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-galaxies [input]
  (let [galaxies (->> input parse-grid (filter #(= (val %) \#)) keys)
        xs (->> galaxies (map first) (into #{}))
        ys (->> galaxies (map second) (into #{}))
        empty-columns (->> xs
                           (apply max)
                           range
                           (remove xs))
        empty-rows (->> ys
                        (apply max)
                        range
                        (remove ys))]
    (map (fn [[x y]]
           [(+ x (->> empty-columns (filter #(< % x)) count))
            (+ y (->> empty-rows (filter #(< % y)) count))])
         galaxies)))

(defn day-11a [input]
  (->> input
       parse-galaxies))

(defn day-11b [input]
  (->> input))
