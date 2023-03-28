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
       ;; NOTE: this will not work if two turns are received in a row
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
      (let [move? (and (= (first current) :move) (> (second current) 0))
            turn? (= (first current) :turn)]
        (recur (if move?
                 (update current 1 dec)
                 (first remaining))
               
               (if move?
                 remaining
                 (rest remaining))
               
               (if move?
                 (let [moved (apply vector (map + position heading))
                       wrapped (if (maze moved) moved (wrap maze position heading))
                       walled (if (= (maze wrapped) :wall) position wrapped)]
                   walled)
                 position)
               
               (if turn?
                 (let [[dx dy] heading]
                   (case (second current)
                     :left [dy (* -1 dx)]
                     :right [(* -1 dy) dx]))
                 heading)))
      
      [position heading])))

(defn day-22a [input]
  (let [[maze instructions] (parse-input input)
        ;; We have inverted `x` and `y` in our coordinate system, so invert them again
        ;; here
        [[column row] heading] (propagate maze instructions)
        facing ({[1 0] 0
                        [0 1] 1
                        [-1 0] 2
                        [0 -1] 3} heading)]
    (+ (* 1000 row)
       (* 4 column)
       facing)))

(defn day-22b [input]
  (->> input))
