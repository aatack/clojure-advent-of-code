(ns advent-of-code-2023.solutions.day-20
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-module [module]
  (let [[name children] (split-string " -> " module)
        broadcaster? (= name "broadcaster")]
    [(if broadcaster? name (subs name 1))
     {:outputs (split-string ", " children)
      :state (cond
               broadcaster? nil
               (= (first name) \%) :off
               :else {})}]))

(defn parse-modules [input]
  (assoc (->> input
              split-lines
              (map parse-module)
              (into {}))
         "button"
         {:state nil :outputs '("broadcaster")}))

(defn day-20a [input]
  (->> input
       parse-modules))

(defn day-20b [input]
  (->> input))
