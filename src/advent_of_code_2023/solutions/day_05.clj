(ns advent-of-code-2023.solutions.day-05
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-mapping [lines]
  (->> lines rest (map #(str "[" % "]")) (map read-string)
       (map #(zipmap [:source :destination :length] %))))

(defn parse-input [input]
  (let [chunks (parse-chunks input)]
    {:seeds (->> chunks
                 first
                 first
                 (split-string "\\:")
                 second
                 (split-string " ")
                 (map read-string)
                 )
     :mappings (->> chunks rest (map parse-mapping))}))

(defn day-05a [input]
  (->> input
       parse-input))

(defn day-05b [input]
  (->> input))
