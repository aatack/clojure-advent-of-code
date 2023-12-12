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

(defn possible-combinations [history block blocks]
  (cond
    (empty? history)
    (if (and (empty? blocks) (or (nil? block) (= block :operational))) 1 0)

    (and (empty? blocks) (nil? block))
    (if (every? #{:operational :unknown} history) 1 0)

    (= (first history) :damaged)
    (case block
      nil (recur history (first blocks) (rest blocks))
      :operational 0
      1 (recur (rest history) :operational blocks)
      (recur (rest history) (dec block) blocks))

    (= (first history) :operational)
    (case block
      :operational (recur (rest history) nil blocks)
      nil (recur (rest history) nil blocks)
      0)

    :else
    (+ (possible-combinations (cons :damaged (rest history)) block blocks)
       (possible-combinations (cons :operational (rest history)) block blocks))))

(defn day-12a [input]
  (->> input
       parse-records
       (take 6)
       (map #(possible-combinations (:history %) nil (:blocks %)))))

(defn day-12b [input]
  (->> input))
