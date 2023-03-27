(ns advent-of-code.solutions.day-20
  (:require [clojure.string :refer [split-lines]]))

(defn parse-file [input]
  (let [lines (split-lines input)
        limit (dec (count lines))]
    (->> lines
         (map-indexed (fn [index value]
                        [index {:distance (read-string value)
                                :left (if (> index 0)
                                        (dec index)
                                        limit)
                                :right (if (< index limit)
                                         (inc index)
                                         0)
                                :index index}]))
         (into {}))))

(defn peek [file value distance]
  (let [direction (if (< distance 0) :left :right)]
    (loop [current-value value
           remaining-distance (abs distance)]
      (if (= remaining-distance 0)
        current-value
        (recur (file (current-value direction))
               (dec remaining-distance))))))

(defn move [file value]
  (let [distance (value :distance)
        index (value :index)

        destination (peek file value distance)
        
        old-left (value :left)
        old-right (value :right)
        
        new-left (if (> distance 0) (destination :index) (destination :left))
        new-right (if (< distance 0) (destination :index) (destination :right))]
    
    (-> file
        ;; Detach the value from its old location
        (assoc-in [old-left :right] old-right)
        (assoc-in [old-right :left] old-left)
        ;; Attach the value to its new location
        (assoc-in [new-left :right] index)
        (assoc-in [new-right :left] index)
        ;; Update the pointers within the value itself
        (assoc-in [index :left] new-left)
        (assoc-in [index :right] new-right))))

(defn mix [file]
  (let [limit (count file)]
    (loop [current-index 0
         current-file file]
    (if (>= current-index limit)
      current-file
      (recur (inc current-index)
             (move current-file (current-file current-index)))))))

(defn day-20a [input]
  (let [file (->> input
                  parse-file
                  mix)
        root (first (filter #(= (% :distance) 0) (vals file)))]
    (apply + (map #((peek file root %) :distance) [1000 2000 3000]))))

(defn day-20b [input]
  (->> input))
