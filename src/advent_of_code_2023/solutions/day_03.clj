(ns advent-of-code-2023.solutions.day-03
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn digit? [character]
  (.contains "0123456789" (str character)))

(defn parse-schematic [input]
  (let [grid (filter (fn [[_ character]] (not= character \.)) (parse-grid input))]
    {:digits (into {} (filter (fn [[_ character]] (digit? character)) grid))
     :symbols (into {} (filter (fn [[_ character]] (not (digit? character))) grid))}))

(defn flag-part-digits [schematic]
  (into {} (map (fn [[[column row] digit]]
                  [[column row]
                   [digit
                    (some (:symbols schematic)
                          (for [x [(dec column) column (inc column)]
                                y [(dec row) row (inc row)]]
                            [x y]))]])
                (:digits schematic))))

(defn find-part-numbers [flagged-digits]
  (->> flagged-digits
       (map
        (fn [[[column row] [digit part?]]]
          (when (not (flagged-digits [(dec column) row]))
            [[column row] [digit part?]])))
       (filter identity)
       (into {})
       (map (fn [[[column row] _]]
              (loop [index column
                     digits []
                     part? false]
                (if-let [[digit digit-part?] (flagged-digits [index row])]
                  (recur (inc index) (conj digits digit) (or part? digit-part?))
                  (when part? [[column row] (read-string (apply str digits))])))))
       (filter identity)
       (into {})))

(defn day-03a [input]
  (->> input
       parse-schematic
       flag-part-digits
       find-part-numbers
       vals
       (apply +)))

(defn find-numbers [schematic]
  (->> (:digits schematic)
       (map
        (fn [[[column row] _]]
          (when (not ((:digits schematic) [(dec column) row]))
            (loop [index column
                   digits []]
              (if-let [digit ((:digits schematic) [index row])]
                (recur (inc index) (conj digits digit))
                [[column row] digits])))))
       (filter identity)
       (into {})))

(defn find-candidate-gears [schematic]
  (map first (filter (fn [[_ character]] (= character \*)) (:symbols schematic))))

(defn number-adjacent-to-gear? [[[number-x number-y] number-digits] [gear-x gear-y]]
  (and (<= (dec number-x) gear-x (+ number-x (count number-digits)))
       (<= (dec number-y) gear-y (inc number-y))))

(defn day-03b [input]
  (let [schematic (parse-schematic input)

        numbers (find-numbers schematic)
        gears (find-candidate-gears schematic)]
    (->>
     gears
     (map (fn [gear]
            (let [adjacent-numbers (filter #(number-adjacent-to-gear? % gear) numbers)]
              (when (= 2 (count adjacent-numbers))
                (apply * (map #(read-string (apply str %)) (vals adjacent-numbers)))))))
     (filter identity)
     (apply +))))
