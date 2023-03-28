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

(defn resolve-monkeys [monkey-context]
  (loop [context monkey-context]
    (let [unevaluated (filter (complement (comp integer? second))
                              context)
          results (for [[monkey function] unevaluated
                        :let [result (function context)]
                        :when result]
                    [monkey result])]
      (if (empty? unevaluated)
        context
        (recur (into context results))))))

(defn day-21a [input]
  ((resolve-monkeys (parse-monkeys input)) "root"))

(defn day-21b [input]
  (->> input))
