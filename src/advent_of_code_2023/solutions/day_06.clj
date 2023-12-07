(ns advent-of-code-2023.solutions.day-06
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

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
   (count (winning-charge-times 30 200))
   )

(defn day-06a [input]
  (->> input))

(defn day-06b [input]
  (->> input))
