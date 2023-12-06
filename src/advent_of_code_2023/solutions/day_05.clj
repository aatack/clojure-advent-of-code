(ns advent-of-code-2023.solutions.day-05
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-mapping [lines]
  (->> lines rest (map #(str "[" % "]")) (map read-string)
       (map #(zipmap [:destination :source :length] %))))

(defn parse-input [input]
  (let [chunks (parse-chunks input)]
    {:seeds (->> chunks
                 first
                 first
                 (split-string "\\:")
                 second
                 (split-string " ")
                 (map read-string))
     :mappings (->> chunks rest (map parse-mapping))}))

(defn apply-mapping [mapping value]
  (if
   (empty? mapping) value
   (let [head (first mapping)
         index (- value (:source head))]
     (if (and (<= 0 index) (< index (:length head)))
       (+ index (:destination head))
       (recur (rest mapping) value)))))

(defn apply-mappings [mappings value]
  (if (empty? mappings)
    value
    (recur (rest mappings) (apply-mapping (first mappings) value))))

(defn day-05a [input]
  (let [{:keys [seeds mappings]} (parse-input input)]
    (->> seeds
         (map #(apply-mappings mappings %))
         (apply min))))

(defn interval-overlaps [[left-start left-end] [right-start right-end]]
  {:below (when (< left-start right-start)
            [left-start (min left-end (dec right-start))])
   :inside (when (not (or (< left-end right-start) (> left-start right-end)))
             [(max left-start right-start) (min left-end right-end)])
   :above (when (> left-end right-end)
            [(max left-start (inc right-end)) left-end])})

(defn apply-interval-mapping [mapping intervals]
  (loop [stages mapping
         waiting intervals
         complete []]
    (if (empty? stages)
      (concat waiting complete)
      (let [[source shift] (first stages)
            overlaps (map #(interval-overlaps % source) waiting)]
        (recur (rest stages)
               (concat (->> overlaps (map :below) (remove nil?))
                       (->> overlaps (map :above) (remove nil?)))
               (concat complete (map (fn [interval] (map #(+ % shift) interval))
                                     (->> overlaps (map :inside) (remove nil?)))))))))

(defn convert-to-interval-mapping [mapping]
  (->> mapping
       (map (fn [{:keys [source destination length]}]
              [[source (dec (+ source length))] (- destination source)]))
       (into {})))

(defn apply-interval-mappings [mappings intervals]
  (if (empty? mappings)
    intervals
    (recur (rest mappings) (apply-interval-mapping (first mappings) intervals))))

(defn parse-seed-ranges [seeds]
  (->> seeds
       (partition 2 2)
       (map (fn [[start length]] [[start (dec (+ start length))]]))))

(defn day-05b [input]
  (let [{:keys [seeds mappings]} (parse-input input)
        interval-mappings (map convert-to-interval-mapping mappings)]
    (->> (parse-seed-ranges seeds)
         (map #(apply-interval-mappings interval-mappings %))
         flatten
         (apply min))))
