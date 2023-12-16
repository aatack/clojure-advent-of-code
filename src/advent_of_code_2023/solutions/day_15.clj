(ns advent-of-code-2023.solutions.day-15
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [includes? split-lines]]))

(defn string-hash [characters]
  (reduce (fn [value character]
            (mod (* 17 (+ value (int character))) 256))
          0
          characters))

(defn day-15a [input]
  (->> input
       (split-string ",")
       (map string-hash)
       (apply +)))

(defn apply-instruction [boxes instruction]
  (if (includes? instruction "-")
    (let [code (subs instruction 0 (dec (count instruction)))]
      (update boxes
              (string-hash code)
              #(into [] (remove (fn [[key]] (= key code)) (or % [])))))
    (let [[code value] (split-string "=" instruction)]
      (update boxes
              (string-hash code)
              #(let [modified (into []
                                    (map (fn [[key focal]]
                                           (if (= key code)
                                             [code (read-string value)]
                                             [key focal]))
                                         (or % [])))]
                 (if (= modified (or % []))
                   (conj modified [code (read-string value)])
                   modified))))))

(defn score [boxes]
  (->> boxes
       (mapcat (fn [[box lenses]] (for [[index [_ focal-length]] (enumerate lenses 1)]
                                    [(inc box) index focal-length])))
       (map #(apply * %))
       (apply +)))

(defn day-15b [input]
  (->> input
       (split-string ",")
       (reduce apply-instruction {})
       score))
