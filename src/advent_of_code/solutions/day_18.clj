(ns advent-of-code.solutions.day-18
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-droplets [input]
  (let [droplets (->> input
                      split-lines
                      (map #(map read-string (split % #",")))
                      (map #(apply vector %))
                      set)]
    (with-meta droplets
      {:bounds (map (fn [dimension]
                      (let [coordinates
                            (map #(nth % dimension) droplets)]
                        [(- (apply min coordinates) 2)
                         (+ (apply max coordinates) 2)]))
                    [0 1 2])})))

(defn neighbours [droplet]
  (for [index [0 1 2]
        direction [inc dec]]
    (update droplet index direction)))

(defn exposed-faces [droplets droplet]
  (filter (complement droplets) (neighbours droplet)))

(defn day-18a [input]
  (let [droplets (parse-droplets input)]
    (apply + (map #(count (exposed-faces droplets %)) droplets))))

(defn project-along-dimensions [droplets dimensions]
  (set (map (fn [droplet]
              (map (fn [dimension]
                     (nth droplet dimension))
                   dimensions))
            droplets)))

(defn in-bounds? [droplets]
  (fn [droplet]
    (every? identity (map (fn [[lower upper] value]
                            (< lower value upper))
                          ((meta droplets) :bounds) droplet))))

(defn propagate-droplet [droplets]
  (let [-in-bounds? (in-bounds? droplets)]
    (fn [droplet]
      (filter #(and (-in-bounds? %)
                    (not (droplets %)))
              (exposed-faces droplets droplet)))))

(defn accumulate-iterations [function initial]
  (loop [queue [initial]
         seen #{initial}]
    (if (empty? queue)
      seen
      (let [new (apply disj
                       (set (function (first queue)))
                       (concat queue seen))]
        (recur (concat (rest queue) new)
               (apply conj seen new))))))

(defn day-18b [input]
  (let [droplets (parse-droplets input)
        airs (accumulate-iterations (propagate-droplet droplets)
                                    [0 0 0])]
    (count (for [droplet droplets
                 air (exposed-faces droplets droplet)
                 :when (airs air)]
             droplet))))
