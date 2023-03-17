(ns advent-of-code.solutions.day-15
  (:require [clojure.string :refer [split-lines replace split]]
            [advent-of-code.utils :refer [conj-range count-ranges]]))

(defn parse-sensors-and-beacons [input]
  (->> (-> input
           (replace #"," "")
           (replace #":" "")
           (replace #"x=" "")
           (replace #"y=" "")
           split-lines)
       (map #(split % #" "))
       (map #(filter integer? (map read-string %)))))

(defn sensor-exclusion-range [[sensor-x sensor-y beacon-x beacon-y] y]
  (let [distance (+ (abs (- sensor-x beacon-x))
                    (abs (- sensor-y beacon-y)))
        width (- distance (abs (- sensor-y y)))]
    (when (>= width 0)
      [(- sensor-x width) (+ sensor-x width)])))

(defn sensors-exclusion-range [sensors y]
  (->> sensors
       (map #(sensor-exclusion-range % y))
       (filter identity)
       (reduce conj-range [])))

(defn day-15a [input]
  (->> input
       parse-sensors-and-beacons
       (map #(sensor-exclusion-range % 2000000))
       (filter identity)
       (reduce conj-range [])
       count-ranges
       ;; One of the spaces in this range is taken up by a sensor,
       ;; which - for whatever reason - isn't counted in the total
       dec))

(defn day-15b [input]
  (let [sensors (parse-sensors-and-beacons input)
        ;; This is slow - and it's not elegant - but it's not *that*
        ;; slow
        rows (for [y (range 4000000)
                   :let [exclusion-range
                         (sensors-exclusion-range sensors y)]
                   :when (> (count exclusion-range) 1)]
               [y exclusion-range])]
    (let [[y [[_ dec-x] [_ _]]] (first rows)
          x (inc dec-x)]
      (+ (* x 4000000) y))))
