(ns advent-of-code-2023.solutions.day-18
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [move-direction split-string]]
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

(defn count-segment-interior
  "For a given slice of a trench, determine the number of interior segments.
   
   This includes the trench itself.  The trench positions should be given as a sequence
   of integers, representing the y-coordinates of each section of trench on a given
   x-coordinate."
  [trench-positions]
  (->> (partition 3 1 [nil] (cons nil trench-positions))
       (remove (fn [[a b c]] (= (inc (or a b)) b (dec (or c b)))))))

(count-segment-interior '(0 1 2 5 6 7))

(defn determine-interior [trench]
  (->> trench
       (group-by first)
       vals
       (map #(sort (map second %)))
       (map count-segment-interior)))

(defn day-18a [input]
  (->> input
       parse-instructions
       build-trench
       determine-interior))

(defn day-18b [input]
  (->> input))
