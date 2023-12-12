(ns advent-of-code-2023.solutions.day-11
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [advent-of-code-2023.utils :refer [unique-pairs]]
            [clojure.string :refer [split-lines]]))

(defn parse-galaxies [expansion-factor input]
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
           [(+ x (* expansion-factor (->> empty-columns (filter #(< % x)) count)))
            (+ y (* expansion-factor (->> empty-rows (filter #(< % y)) count)))])
         galaxies)))

(defn distance [[left-x left-y] [right-x right-y]]
  (+ (abs (- left-x right-x)) (abs (- left-y right-y))))

(defn day-11a [input]
  (->> input
       (parse-galaxies 1)
       unique-pairs
       (map #(apply distance %))
       (apply +)))

(defn day-11b [input]
  (->> input
       (parse-galaxies 1000000)
       unique-pairs
       (map #(apply distance %))
       (apply +)))
