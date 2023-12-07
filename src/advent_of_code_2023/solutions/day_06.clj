(ns advent-of-code-2023.solutions.day-06
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.math :refer [sqrt]]
            [clojure.string :refer [split-lines]]))

(defn parse-races [input]
  (let [[times distances] (split-lines input)]
    (zipmap (->> times (split-string " ") rest (map read-string))
            (->> distances (split-string " ") rest (map read-string)))))

(defn distance [charge-time race-time]
  (* (- race-time charge-time) charge-time))

(defn winning-charge-times [race-time race-distance]
  (->> (range (inc race-time))
       (map #(distance % race-time))
       (filter #(> % race-distance))))

(defn day-06a [input]
  (->> input
       parse-races
       (map #(apply winning-charge-times %))
       (map count)
       (apply *)))

(defn parse-race [input]
  (let [[times distances] (split-lines input)]
    [(->> times (split-string " ") rest (apply str) read-string)
     (->> distances (split-string " ") rest (apply str) read-string)]))

(defn required-charge-time
  "Find the charge time required to win the race.
   
   The winning distance `d`, charge time `c`, and race time `r` must satisfy the
   following inequality:
   
     d > (r - c)c ==> c^2 - rc + d > 0
   
   Hence we can solve for `c` to find the two roots, which are the charge times between
   which the race will be won.  We can take a shortcut to that value by simply doubling
   the scale - the distance between the maximum/minimum and each root - and rounding it
   down."
  [race-time race-distance]
  (int (sqrt (- (* race-time race-time) (* 4 race-distance)))))

(defn day-06b [input]
  (->> input
       parse-race
       (apply required-charge-time)))
