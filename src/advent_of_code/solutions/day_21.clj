(ns advent-of-code.solutions.day-21
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-monkey [input]
  (let [[monkey operation] (split input #": ")
        expression (split operation #" ")]
    [monkey (if (= (count expression) 1)
              (read-string (first expression))
              (let [[left operator right] expression]
                (fn [context]
                  (try (({"+" +
                          "-" -
                          "*" *
                          "/" /} operator)
                        (context left)
                        (context right))
                       (catch Exception _)))))]))

(defn parse-monkeys [input]
  (->> input
       split-lines
       (map parse-monkey)
       (into {})))

(defn day-21a [input]
  (loop [context (parse-monkeys input)]
    (let [unevaluated (set (map first
                                (filter (complement (comp integer? second))
                                        context)))]
      unevaluated)))

(defn day-21b [input]
  (->> input))
