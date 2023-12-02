(ns advent-of-code-2023.solutions.day-01
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

(defn calibration-value [characters]
  (let [numbers (->> characters
                     (filter #(re-matches #"[0-9]" (str %))))]
    (read-string (str (first numbers) (last numbers)))))

(defn day-01a [input]
  (->> input
       split-lines
       (map calibration-value)
       (apply +)))

(def lookups {"0" 0
              "1" 1
              "2" 2
              "3" 3
              "4" 4
              "5" 5
              "6" 6
              "7" 7
              "8" 8
              "9" 9
              "zero" 0
              "one" 1
              "two" 2
              "three" 3
              "four" 4
              "five" 5
              "six" 6
              "seven" 7
              "eight" 8
              "nine" 9})

(defn spelled-calibration-value [characters]
  ,,,)

(defn day-01b [input]
  (->> input))
