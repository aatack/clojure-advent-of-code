(ns advent-of-code-2023.solutions.day-12
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-record [record]
  (let [[history blocks] (split-string " " record)]
    {:history (map {\. :operational \# :damaged \? :unknown} history)
     :blocks (read-string (str "[" blocks "]"))}))

(defn parse-records [input]
  (->> input
       split-lines
       (map parse-record)))

(defn day-12a [input]
  (->> input
       parse-records))

(defn day-12b [input]
  (->> input))
