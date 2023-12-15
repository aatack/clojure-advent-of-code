(ns advent-of-code-2023.solutions.day-14
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-platform [input]
  (let [platform (parse-grid input)]
    {:spaces (->> platform
                  (filter #(#{\O \.} (val %)))
                  keys
                  (into #{}))
     :rocks (->> platform
                 (filter #(#{\O} (val %)))
                 keys
                 (into #{}))}))

(defn add [left right]
  (into [] (map + left right)))

(defn scale [direction magnitude]
  (into [] (map #(* % magnitude)) direction))

(defn dot [left right]
  (apply + (map * left right)))

(defn tip-rock [platform direction rock]
  (let [rocks (disj (:rocks platform) rock)
        steps (take-while #(let [position (add rock (scale direction %))]
                             (and ((:spaces platform) position) (not (rocks position))))
                          (range))]
    (assoc platform :rocks (conj rocks (add rock (scale direction (last steps)))))))

(defn tip [platform direction]
  (reduce (fn [tipped-platform rock]
            (tip-rock tipped-platform direction rock))
          platform
          (sort-by #(* -1 (dot % direction)) (:rocks platform))))

(defn day-14a [input]
  (-> input
      parse-platform
      (tip [0 -1])))

(defn day-14b [input]
  (->> input))
