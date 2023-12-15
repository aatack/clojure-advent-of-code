(ns advent-of-code-2023.solutions.day-14
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-platform [input]
  (let [platform (parse-grid input)]
    {:spaces (->> platform
                  (filter #(#{\O \.} (val %)))
                  keys)
     :rocks (->> platform
                 (filter #(#{\O} (val %)))
                 keys)}))

(defn day-14a [input]
  (->> input
       parse-platform))

(defn day-14b [input]
  (->> input))
