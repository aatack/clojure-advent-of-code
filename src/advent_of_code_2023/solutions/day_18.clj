(ns advent-of-code-2023.solutions.day-18
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.solutions.day-14 :refer [add scale]]
            [advent-of-code-2023.solutions.day-16 :refer [explore]]
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

(defn build-vertices [instructions]
  (reductions (fn [vertex {:keys [direction distance]}]
                (add vertex (scale (move-direction direction [0 0]) distance)))
              [0 0]
              instructions))

(defn polygon-area [vertices]
  (->> vertices
       (partition 2 1)
       (map (fn [[[left-x left-y] [right-x right-y]]]
              (let [dx (- right-x left-x)
                    dy (- right-y left-y)]
                (* (+ (* (abs dx) (min left-y right-y))
                      (/ (* (abs dx) (abs dy)) 2))
                   (if (< dx 0) 1 -1)))))
       (apply +)
       abs))

(defn day-18a [input]
  (let [instructions (->> input parse-instructions)]
    (+ (->> instructions build-vertices polygon-area)
       ;; Adjust the area of the polygon for the fact that we're working with solid
       ;; squares instead of points.  On straight lines, each step on the perimeter
       ;; passes through an additional half a block that's not counted by the polygon
       ;; approximation.  Internal and external corners count for 3/4 and 1/4 of a block
       ;; each, and should cancel out; except that the number of external corners must
       ;; be four greater than the number of internal corners (to make a loop), so we
       ;; need to add an additional one block
       (->> instructions (map :distance) (apply +) (* 0.5) int inc))))

(defn parse-colour [colour]
  {:direction (case (last colour)
                \0 :right
                \1 :down
                \2 :left
                \3 :up)
   :distance (read-string (apply str "0x" (butlast colour)))})

(defn day-18b [input]
  (let [instructions (->> input parse-instructions (map :colour) (map parse-colour))]
    (+ (->> instructions build-vertices polygon-area)
       (->> instructions (map :distance) (apply +) (* 0.5) int inc))))
