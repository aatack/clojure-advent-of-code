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

(defn day-15b
  "Find the only location that the sensor can be.
   
   This uses a simple brute force search.  To solve it more efficiently,
   define a data structure to represent a diamond (ie. rectangle, but
   aligned along the diagonals rather than along the axes).  Each sensor
   represents a hole carved out of the remaining possibilities, whose
   shape is also a diamond.
   
   Start with a diamond that encompasses the whole area, and then for
   each sensor, split the diamond into five: first along two parallel
   edges, then along the other two.  Discard the fifth, innermost
   diamond.  The remaining pieces can still all be expressed in terms
   of the diamond data structure.
   
   As the sensors are worked through, some of the remaining pieces of
   the possible space of beacons will be carved away to nothing, and
   can be removed.  In principle, at the end, there will only be one
   diamond, which will have an area of 1.  Those are the coordinates of
   the beacon."
  [input]
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
