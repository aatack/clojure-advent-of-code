(ns advent-of-code-2023.solutions.day-16
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn move [direction [x y]]
  (case direction
    :left [(dec x) y]
    :right [(inc x) y]
    :up [x (dec y)]
    :down [x (inc y)]))

(defn propagate [mirror direction]
  (case mirror
    \. [direction]
    \\ (case direction
         :right [:down]
         :down [:right]
         :up [:left]
         :left [:up])
    \/ (case direction
         :right [:up]
         :down [:left]
         :up [:right]
         :left [:down])
    \| (if (#{:left :right} direction)
         [:up :down]
         [direction])
    \- (if (#{:up :down} direction)
         [:left :right]
         [direction])
    nil []))

(defn explore [children initial]
  (loop [unexplored #{initial}
         explored #{}]
    (if (empty? unexplored)
      explored
      (let [node (first unexplored)
            new-nodes (->> (children node) (remove explored) (remove unexplored))]
        (recur (into #{} (apply conj (rest unexplored) new-nodes))
               (conj explored node))))))

(defn energised
  ([grid] (energised grid [:right [-1 0]]))
  ([grid start]
   (->> start
        (explore (fn [[direction position]]
                   (let [new-position (move direction position)]
                     (map #(vector % new-position)
                          (propagate (grid new-position) direction)))))
        (map second)
        (into #{})
        count
       ;; Remove one because we start outside the board
        dec)))

(defn day-16a [input]
  (->> input
       parse-grid
       energised))

(defn day-16b [input]
  (let [grid (->> input parse-grid)
        columns (->> grid keys (map first) (into #{}))
        rows (->> grid keys (map second) (into #{}))
        starts (concat
                (for [column columns
                      [direction row] {:down -1, :up (inc (apply max rows))}]
                  [direction [column row]])
                (for [row rows
                      [direction column] {:right -1, :left (inc (apply max columns))}]
                  [direction [column row]]))]
    (apply max (map #(energised grid %) starts))))
