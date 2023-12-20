(ns advent-of-code-2023.solutions.day-18
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.solutions.day-16 :refer [explore]]
            [advent-of-code-2023.utils :refer [move-direction split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-instructions [input]
  (->> input
       split-lines
       (map #(-> %
                 (clojure.string/replace #"\(" "")
                 (clojure.string/replace #"\)" "")
                 (clojure.string/replace #"#" "")))
       (map #(split-string " " %))
       (map (fn [[direction distance colour]]
              {:direction ({"R" :right
                            "U" :up
                            "L" :left
                            "D" :down} direction)
               :distance (read-string distance)
               :colour colour}))))

(defn build-trench [instructions]
  (->>
   (reduce (fn [trench instruction]
             (cons (with-meta (move-direction (:direction instruction) (first trench))
                     {:instruction instruction})
                   trench))
           '([0 0])
           (mapcat #(repeat (:distance %) %) instructions))
   (into #{})))

(defn determine-interior [trench]
  (explore (fn [node]
             (remove trench [(move-direction :up node)
                             (move-direction :down node)
                             (move-direction :left node)
                             (move-direction :right node)]))
           [1 1]))

(defn day-18a [input]
  (let [trench (->> input parse-instructions build-trench)]
    (+ (count trench) (count (determine-interior trench)))))

(defn day-18b [input]
  (->> input))
