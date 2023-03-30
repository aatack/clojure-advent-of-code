(ns advent-of-code.solutions.day-22
  (:require [advent-of-code.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(def side-length 50)

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

(def right [1 0])
(def left [-1 0])
(def up [0 -1])
(def down [0 1])

(def directions [left right up down])

(defn sector
  "Find the sector, relative to `side-length`, within which a position vector resides."
  [coordinate]
  (apply vector (map #(quot (dec %) side-length) coordinate)))

(defn rotate
  "Rotate a position vector from one heading to another."
  ([position from to]
  (loop [[x y] position
         [dx dy] from]
    (if (= [dx dy] to)
      [x y]
      (recur [y (* -1 x)]
             [dy (* -1 dx)]))))
  
  ([position to]
   (rotate position up to)))

(defn add
  "Add two position vectors together."
  [left-position right-position]
  (apply vector (map + left-position right-position)))

(defn scale
  "Scale a position vectory by a constant."
  [position constant]
  (apply vector (map #(+ % constant) position)))

(defn focus
  "Move the given position vector into a `side-length` sized square around the origin."
  [position]
  (apply vector (map #(inc (mod (dec %) side-length)) position)))

(defn parse-cube [maze]
  (let [sectors (group-by sector (keys maze))]
    (loop [cube (into
                 {}
                 (map (fn [[face positions]]
                        [face
                         {:map (into {}
                                     (map (fn [position]
                                            [(focus position) (maze position)])
                                          positions))
                          ;; Maps each direction to the connected sector
                          left nil
                          right nil
                          up nil
                          down nil}])
                      sectors))]

      (let [missing (for [face (keys cube)
                          direction directions
                          :when (nil? (get-in cube [face direction]))]
                      [face direction])
            connections (concat
                         (for [face (keys cube)
                               direction directions
                               :let [other-face (add face direction)]
                               :when (cube other-face)]
                           [face direction other-face])
                         (for [face (keys cube)
                               direction directions
                               corner [left right]
                               :let [near-face-direction (rotate direction corner)
                                     near-face (get-in cube [face near-face-direction])]
                               :when near-face
                               :let [far-face (get-in cube
                                                      [near-face
                                                       (rotate near-face-direction
                                                               (if (= corner left)
                                                                 left
                                                                 right))])]
                               :when (cube far-face)]
                           [face direction far-face]))]

        (if (empty? missing)
          cube

          ;; Work out which connections can be inferred from the current information
          (recur (reduce (fn [cube' [from direction to]]
                           (assoc-in cube' [from direction] to))
                         cube
                         connections)))))))

(defn day-22b [input]
  (let [[maze instructions] (parse-input input)]
    (parse-cube maze)))
