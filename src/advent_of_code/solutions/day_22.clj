(ns advent-of-code.solutions.day-22
  (:require [advent-of-code.utils :refer [enumerate]]
            [clojure.string :refer [split-lines]]))

(def side-length 4)

(def right [1 0])
(def left [-1 0])
(def up [0 -1])
(def down [0 1])

(def directions [left right up down])
(def scores {[1 0] 0
                 [0 1] 1
                 [-1 0] 2
                 [0 -1] 3})

(defn parse-maze [input]
  (into {} (for [[y row] (enumerate input)
                 [x cell] (enumerate row)
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
         position (apply min-key first (filter (comp #(= % 0) second) (keys maze)))
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
  ;; TODO: this needs to be fixed after the shift to 0-indexed coordinates (which, it
  ;;       transpires, was actually unnecessary anyway)
  (let [[maze instructions] (parse-input input)
        ;; We have inverted `x` and `y` in our coordinate system, so invert them again
        ;; here
        [[column row] heading] (propagate maze instructions)
        facing (scores heading)]
    (+ (* 1000 (inc row))
       (* 4 (inc column))
       facing)))

(defn sector
  "Find the sector, relative to `side-length`, within which a position vector resides."
  [coordinate]
  (apply vector (map #(quot % side-length) coordinate)))

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
  (apply vector (map #(* % constant) position)))

(defn rotate-face
  "Rotate a position in the face coordinate space."
  [position from to]
  (let [centre (/ (dec side-length) 2)
        pivot [centre centre]]
    (->> (-> position
             (add (scale pivot -1))
             (rotate from to)
             (add pivot))
         (map int)
         (apply vector))))

(defn focus
  "Move the given position vector into a `side-length` sized square around the origin."
  [position]
  (apply vector (map #(mod % side-length) position)))

(defn find-direction [cube from-face to-face]
  (first (for [direction directions
               :when (= (get-in cube [from-face direction]) to-face)]
           direction)))

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
                               :let [near-face (get-in
                                                cube
                                                [face (rotate direction corner)])]
                               :when near-face
                               :let [far-face
                                     (get-in cube
                                             [near-face
                                              (rotate
                                               (find-direction cube near-face face)
                                               corner)])]
                               :when (cube far-face)]
                           [face direction far-face]))]

        (if (empty? missing)
          (assoc cube :maze maze)

          ;; Work out which connections can be inferred from the current information
          (recur (reduce (fn [cube' [from direction to]]
                           (assoc-in cube' [from direction] to))
                         cube
                         connections)))))))

(defn orient [cube position from-face to-face]
  (-> position
      (rotate-face (find-direction cube from-face to-face)
                   (scale (find-direction cube to-face from-face) -1))
      (add (scale (find-direction cube to-face from-face) side-length))))

(defn move-once [cube face position heading]
  (let [moved (add position heading)]
    (case (get-in cube [face :map moved])
      :open [face moved heading]
      :wall [face position heading]
      (let [target-face (get-in cube [face heading])
            target-position (orient cube moved face target-face)
            target-heading (scale (find-direction cube target-face face) -1)]

        (if (= (get-in cube [target-face :map target-position]) :wall)
          [face position heading]
          [target-face target-position target-heading])))))

(defn propagate [cube instructions]
  (let [initial-position (apply min-key first
                                (filter (comp #(= % 0) second) (keys (cube :maze))))]
    (loop [current-instruction (first instructions)
           remaining-instructions (rest instructions)
           current-face (sector initial-position)
           current-position (focus initial-position)
           current-heading right]

      (if current-instruction
        (let [move? (and (= (first current-instruction) :move)
                         (> (second current-instruction) 0))
              turn? (= (first current-instruction) :turn)

              [next-face next-position next-heading]
              (if move?
                (move-once cube
                           current-face
                           current-position
                           current-heading)
                [current-face
                 current-position
                 (if turn?
                   (rotate current-heading
                           (if (= (second current-instruction) :left)
                             left
                             right))
                   current-heading)])]

          (recur (if move?
                   (update current-instruction 1 dec)
                   (first remaining-instructions))

                 (if move?
                   remaining-instructions
                   (rest remaining-instructions))

                 next-face
                 next-position
                 next-heading))

        [current-face current-position current-heading]))))

(defn day-22b [input]
  (let [[maze instructions] (parse-input input)
        cube (parse-cube maze)
        [[face-x face-y] [position-x position-y] heading] (propagate cube instructions)
        column (inc (+ position-x (* face-x side-length)))
        row (inc (+ position-y (* face-y side-length)))]
    (+ (* 1000 row)
       (* 4 column)
       (scores heading))))
