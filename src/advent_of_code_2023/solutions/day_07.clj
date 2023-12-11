(ns advent-of-code-2023.solutions.day-07
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-hands [input]
  (->> input
       split-lines
       (map #(split-string " " %))
       (map (fn [[hand bid]] {:hand (map identity hand) :big (read-string bid)}))))

(defn card-counts [hand]
  (->> hand
       (reduce (fn [counts card]
                 (update counts card #(inc (or % 0))))
               {})
       vals
       sort
       reverse))

(defn day-07a [input]
  (->> input
       parse-hands
       first
       :hand
       card-counts))

(defn day-07b [input]
  (->> input))
