(ns advent-of-code-2023.solutions.day-06
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.math :refer [sqrt]]
            [clojure.string :refer [split-lines]]))

(defn distance [charge-time race-time]
  (* (- race-time charge-time) charge-time))

(defn winning-charge-times [race-time race-distance]
  (->> (range (inc race-time))
       (map #(distance % race-time))
       (filter #(> % race-distance))))

(* (count (winning-charge-times 48 390))
   (count (winning-charge-times 98 1103))
   (count (winning-charge-times 90 1112))
   (count (winning-charge-times 83 1360)))

(* (count (winning-charge-times 7 9))
   (count (winning-charge-times 15 40))
   (count (winning-charge-times 30 200)))

(defn day-06a [input]
  (->> input))

(defn required-charge-time
  "Find the charge time required to win the race.
   
   The winning distance `d`, charge time `c`, and race time `r` must satisfy the
   following inequality:
   
     d > (r - c)c ==> c^2 - rc + d > 0
   
   Hence we can solve for `c` to find the two roots, which are the charge times between
   which the race will be won."
  [race-time race-distance]
  (let [a 1
        b (* -1 race-time)
        c race-distance

        denominator (* 2 a)
        location (/ (* -1 b) denominator)
        scale (/ (sqrt (- (* b b) (* 4 a c))) denominator)]
    (int (* 2 scale))))

(required-charge-time  48989083 390110311121360)

(defn day-06b [input]
  (->> input))
