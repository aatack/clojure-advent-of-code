(ns advent-of-code.solutions.day-22
  (:require [advent-of-code.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(defn parse-maze [input]
  (into {} (for [[y row] (enumerate input 1)
                 [x cell] (enumerate row 1)
                 :when (not= cell \space)]
             [[x y] (if (= cell \.) :open :wall)])))

(defn parse-instructions [input]
  (->> input
       (partition-by #{\L \R})
       (map (fn [instruction]
              (cond
                (= (first instruction) \L) [:turn :left]
                (= (first instruction) \R) [:turn :right]
                :else [:move (read-string (apply str instruction))])))))

(defn parse-input [input]
  (let [[maze _ [instructions]] (->> input split-lines (partition-by empty?))]
    [(parse-maze maze) (parse-instructions instructions)]))

(defn wrap [maze coordinate direction]
  (let [step (apply vector (map #(* -1 %) direction))]
    (loop [current-coordinate coordinate]
      (let [next-coordinate (apply vector (map + current-coordinate step))]
        (if (maze next-coordinate)
          (recur next-coordinate)
          current-coordinate)))))

(defn propagate [maze instructions]
  (loop [current (first instructions)
         remaining (rest instructions)
         position (apply min-key first (filter (comp #(= % 1) second) (keys maze)))
         heading [1 0]]
    (if current
      (recur (first remaining)
             (rest remaining)
             (if (= (first current) :move)
               position
               position)
             (if (= (first current) :turn)
               heading
               heading))
      [position heading])))

(defn day-22a [input]
  (let [[maze instructions] (parse-input input)]
    (propagate maze instructions)))

(defn day-22b [input]
  (->> input))
