(ns advent-of-code-2022.solutions.day-20
  (:require [clojure.string :refer [split-lines]]))

(def encryption-key 811589153)

(defn parse-file [factor input]
  (let [lines (split-lines input)
        limit (dec (count lines))]
    (->> lines
         (map-indexed (fn [index value]
                        [index {:distance (*' factor (read-string value))
                                :left (if (> index 0)
                                        (dec index)
                                        limit)
                                :right (if (< index limit)
                                         (inc index)
                                         0)
                                :index index}]))
         (into {}))))

(defn peek-file [file value distance]
  (let [direction (if (< distance 0) :left :right)]
    (loop [current-value value
           remaining-distance (abs distance)]
      (if (= remaining-distance 0)
        current-value
        (recur (file (current-value direction))
               (dec remaining-distance))))))

(defn move [file value]
  (let [old-left (value :left)
        old-right (value :right)

        removed (-> file
                    ;; Detach the value from its old location
                    (assoc-in [old-left :right] old-right)
                    (assoc-in [old-right :left] old-left))

        distance (mod (value :distance) (* (dec (count file)) (if (> (value :distance) 0) 1 -1)))
        index (value :index)

        destination (peek-file removed value distance)

        new-left (if (> distance 0) (destination :index) (destination :left))
        new-right (if (< distance 0) (destination :index) (destination :right))]

    (if (= distance 0)
      file
      (-> removed
          ;; Attach the value to its new location
          (assoc-in [new-left :right] index)
          (assoc-in [new-right :left] index)
          ;; Update the pointers within the value itself
          (assoc index (-> value
                           (assoc :left new-left)
                           (assoc :right new-right)))))))

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
                  (parse-file 1)
                  mix)
        root (first (filter #(= (% :distance) 0) (vals file)))]
    (apply + (map #((peek-file file root %) :distance) [1000 2000 3000]))))

(defn day-20b [input]
  (let [file (->> input
                  (parse-file encryption-key)
                  (iterate mix)
                  (drop 10)
                  first)
        root (first (filter #(= (% :distance) 0) (vals file)))]
    (apply +' (map #((peek-file file root %) :distance) [1000 2000 3000]))))
