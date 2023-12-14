(ns advent-of-code-2023.solutions.day-13
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [clojure.string :refer [split-lines]]))

(defn reflection-row [lines]
  (loop [above [(first lines)]
         below (rest lines)]
    (cond 
      (empty? below) nil
      (every? identity (map = above below)) [(count above) above below]
      :else (recur (cons (first below) above) (rest below)))))

(defn day-13a [input]
  (->> input
       parse-chunks))

(defn day-13b [input]
  (->> input))


(map str "Hello" "World")
