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

;; This approach would probably break if you had three chained together, though that
;; doesn't appear to happen anywhere in the inputs.  To fix this, you could instead go
;; through each lookup and find the indices at which it appears in the input string.
;; Then take the first and last of these, after sorting by index
(def lookups [["oneight" [1 8]]
              ["twone" [2 1]]
              ["threeight" [3 8]]
              ["fiveight" [5 8]]
              ["sevenine" [7 9]]
              ["eightwo" [8 2]]
              ["eighthree" [8 3]]
              ["nineight" [9 8]]
              ["1" [1]]
              ["2" [2]]
              ["3" [3]]
              ["4" [4]]
              ["5" [5]]
              ["6" [6]]
              ["7" [7]]
              ["8" [8]]
              ["9" [9]]
              ["one" [1]]
              ["two" [2]]
              ["three" [3]]
              ["four" [4]]
              ["five" [5]]
              ["six" [6]]
              ["seven" [7]]
              ["eight" [8]]
              ["nine" [9]]])

(defn spelled-calibration-value [characters]
  (loop [lookups' lookups
         characters' characters
         numbers []]
    (cond (= characters' "") (read-string (str (first numbers) (last numbers)))
          (empty? lookups') (recur lookups (subs characters' 1) numbers)
          :else
          (let [[lookup value] (first lookups')
                length (count lookup)
                matches? (and (>= (count characters') length)
                              (= (subs characters' 0 length) lookup))]
            (if matches?
              (recur lookups (subs characters' length) (concat numbers value))
              (recur (rest lookups') characters' numbers))))))

(defn day-01b [input]
  (->> input
       split-lines
       (map spelled-calibration-value)
       (apply +)))
