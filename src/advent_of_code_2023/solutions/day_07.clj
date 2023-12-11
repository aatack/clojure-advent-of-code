(ns advent-of-code-2023.solutions.day-07
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-hands [input]
  (->> input
       split-lines
       (map #(split-string " " %))
       (map (fn [[hand bid]] {:hand (map identity hand) :bid (read-string bid)}))))

(defn card-counts [hand]
  (->> hand
       (reduce (fn [counts card]
                 (update counts card #(inc (or % 0))))
               {})
       vals
       sort
       reverse))

(def type-score {[5] 5
                 [4 1] 4
                 [3 2] 3
                 [3 1 1] 2
                 [2 2 1] 1
                 [2 1 1 1] 0
                 [1 1 1 1 1] -1})

(def card-score {\A 14
                 \K 13
                 \Q 12
                 \J 11
                 \T 10
                 \9 9
                 \8 8
                 \7 7
                 \6 6
                 \5 5
                 \4 4
                 \3 3
                 \2 2
                 \1 1})

(defn overall-score [hand]
  [(type-score (card-counts hand)) (into [] (map card-score hand))])

(defn sort-hands [get-hand items]
  (sort-by (comp overall-score get-hand) compare items))

(defn day-07a [input]
  (->> input
       parse-hands
       (sort-hands :hand)
       enumerate
       (map (fn [[index {:keys [bid]}]]
              (* bid (inc index))))
       (apply +)))

(defn day-07b [input]
  (->> input))

(sort-by :x compare [{:x 3} {:x 2} {:x 5}])
