(ns advent-of-code-2023.solutions.day-19
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [clojure.string :refer [split-lines]]))

(defn parse-workflow [workflow]
  workflow)

(defn parse-part [part]
  (read-string (clojure.string/replace part #"=" " ")))

(defn day-19a [input]
  (let [[workflows-input parts-input] (parse-chunks input)
        parts (map parse-part parts-input)
        workflows (map parse-workflow workflows-input)]
    parts))

(defn day-19b [input]
  (->> input))
