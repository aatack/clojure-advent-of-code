(ns advent-of-code.solutions.day-05
  (:require [clojure.string :refer [split-lines split]]))

(defn transpose [sequences]
  (apply map vector sequences))

(defn split-input
  "Splits input into stack and procedure sections."
  [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)))

(defn parse-stack [stack]
  (->> stack
       butlast
       (map rest)
       (map #(take-nth 4 %))
       (map #(apply str %))
       transpose
       (map #(drop-while (fn [character] (= character \space)) %))))

(defn parse-procedure [procedure]
  (->> procedure
       (map #(split % #" "))
       (map rest)
       (map #(take-nth 2 %))
       (map #(map read-string %))
       (map (fn [[move from to]]
              {:move move :from from :to to}))))

(defn day-05a [input]
  (let [[stack-input procedure-input] (split-input input)
        stack (parse-stack stack-input)
        procedure (parse-procedure procedure-input)]
    (parse-procedure procedure-input)))

(defn day-05b [input]
  (->> input))
