(ns advent-of-code-2023.solutions.day-12
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-record [record]
  (let [[history blocks] (split-string " " record)]
    {:history (map {\. :operational \# :broken \? :unknown} history)
     :blocks (read-string (str "[" blocks "]"))}))

(defn parse-records [input]
  (->> input
       split-lines
       (map parse-record)))

(defn possible-combinations [history block blocks]
  (cond
    (empty? history)
    (if (and (empty? blocks) (or (nil? block) (= block :broken))) 1 0)

    ;; (and (empty? blocks) (nil? block))
    ;; 9999

    (= (first history) :operational)
    (case block
      nil (recur history (first blocks) (rest blocks))
      :broken 0
      1 (recur (rest history) :broken blocks)
      (recur (rest history) (dec block) blocks))

    (= (first history) :broken)
    (case block
      :broken (recur (rest history) nil blocks)
      nil (recur (rest history) nil blocks)
      0)

    :else
    (+ (possible-combinations (cons :operational (rest history)) block blocks)
       (possible-combinations (cons :broken (rest history)) block blocks))))

(defn day-12a [input]
  (->> input
       parse-records
       (take 1)
       #_(map #(possible-combinations (:history %) nil (:blocks %)))))

(defn day-12b [input]
  (->> input))
