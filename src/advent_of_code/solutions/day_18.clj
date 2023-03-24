(ns advent-of-code.solutions.day-18
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-droplets [input]
  (->> input
       split-lines
       (map #(map read-string (split % #",")))
       (map #(apply vector %))
       set))

(defn neighbours [droplet]
  (for [index [0 1 2]
        direction [inc dec]]
    (update droplet index direction)))

(defn exposed-faces [droplets droplet]
  (count (filter (complement droplets) (neighbours droplet))))

(defn day-18a [input]
  (let [droplets (parse-droplets input)]
    (apply + (map #(exposed-faces droplets %) droplets))))

(defn project-along-dimensions [droplets dimensions]
  (set (map (fn [droplet]
         (map (fn [dimension]
                (nth droplet dimension))
              dimensions))
       droplets)))

(defn day-18b [input]
  (->> input))
