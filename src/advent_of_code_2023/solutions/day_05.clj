(ns advent-of-code-2023.solutions.day-05
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-mapping [lines]
  (->> lines rest (map #(str "[" % "]")) (map read-string)
       (map #(zipmap [:destination :source :length] %))))

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

(defn apply-mapping [mapping value]
  (if
    (empty? mapping) value
    (let [head (first mapping)
          index (- value (:source head))]
      (if (and (<= 0 index) (< index (:length head)))
        (+ index (:destination head))
        (recur (rest mapping) value)))))

(defn apply-mappings [mappings value]
  (if (empty? mappings)
    value
    (recur (rest mappings) (apply-mapping (first mappings) value))))

(defn day-05a [input]
  (let [{:keys [seeds mappings]} (parse-input input)]
    (->> seeds
         (map #(apply-mappings mappings %))
         (apply min))))

(defn day-05b [input]
  (->> input))
