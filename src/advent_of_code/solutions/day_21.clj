(ns advent-of-code.solutions.day-21
  (:require [clojure.string :refer [split split-lines]]))

(defn wrap-operator [operator]
  (fn [left right]
    (when (and left right)
      (try (operator left right)
       (catch Exception _ (operator (double left) (double right)))))
  ))

(defn parse-monkey [equality input]
  (let [[monkey operation] (split input #": ")
        expression (split operation #" ")]
    [monkey (if (= (count expression) 1)
              (read-string (first expression))
              (let [[left operator right] expression]
                (fn [context]
                  (try ((wrap-operator (if (and equality (= monkey "root"))
                          -'
                          ({"+" +'
                            "-" -'
                            "*" *'
                            "/" /} operator)))
                        (context left)
                        (context right))
                       (catch Exception _)))))]))

(defn parse-monkeys [equality input]
  (->> input
       split-lines
       (map #(parse-monkey equality %))
       (into {})))

(defn resolve-monkeys [monkey-context]
  (loop [context monkey-context]
    (let [unevaluated (filter (complement (comp #(or (integer? %) (double? %)) second))
                              context)
          results (for [[monkey function] unevaluated
                        :let [result (function context)]
                        :when result]
                    [monkey result])]
      (if (empty? unevaluated)
        context
        (recur (into context results))))))

(defn day-21a [input]
  ((resolve-monkeys (parse-monkeys false input)) "root"))

(defn attempt [context]
  (fn [value]
    (let [augmented-context (assoc context "humn" value)]
      [value ((resolve-monkeys augmented-context) "root")])))

(defn day-21b [input]
  (let [context (parse-monkeys true input)
        tries [301]]
    (into {} (filter second (map (attempt context) tries)))))
