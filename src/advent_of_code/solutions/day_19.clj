(ns advent-of-code.solutions.day-19
  (:require [clojure.string :refer [split-lines replace split]]))

(defn parse-cost [input]
  (->> (split input #",")
       (map #(split % #" "))
       (map (fn [[quantity material]]
              [(read-string (str ":" material))
               (read-string quantity)]))
       (into {})))

(defn parse-blueprint [input]
  (let [[id ore clay obsidian geode]
        (-> input
            (replace "Blueprint " "")
            (replace ": Each ore robot costs " ";")
            (replace ". Each clay robot costs " ";")
            (replace ". Each obsidian robot costs " ";")
            (replace ". Each geode robot costs " ";")
            (replace " and " ",")
            (replace "." "")
            (split #";"))]
    {:id (read-string id)
     :ore (parse-cost ore)
     :clay (parse-cost clay)
     :obsidian (parse-cost obsidian)
     :geode (parse-cost geode)}))

(defn parse-blueprints [input]
  (->> input
       split-lines
       (map parse-blueprint)))

(defn day-19a [input]
  (->> input
       parse-blueprints))

(defn day-19b [input]
  (->> input))
