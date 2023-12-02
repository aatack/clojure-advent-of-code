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

(defn day-01b [input]
  (->> input))
