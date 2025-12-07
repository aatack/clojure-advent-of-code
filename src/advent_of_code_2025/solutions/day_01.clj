(ns advent-of-code-2025.solutions.day-01
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require
   [advent-of-code-2025.helpers :refer [integers-between]]
   [clojure.math :refer [ceil floor]]
   [clojure.string :refer [split-lines]]))

(def default-state {:value 50 :count 0})

(defn update-state [state command]
  (let [direction (if (= (first command) \L) -1 1)
        amount (read-string (subs command 1))
        new-value (-> state
                      :value
                      (+ (* direction amount))
                      (mod 100))]
    (-> state
        (assoc :value new-value)
        (update :count (if (= new-value 0) inc identity)))))

(defn day-01a [input]
  (->> input
       split-lines
       (reduce update-state default-state)
       :count))

(defn rotations-through-zero [old-value new-value]
  (->> (integers-between old-value new-value)
       rest
       (filter #(= (mod % 100) 0))
       count))

(defn update-state-comprehensive [state command]
  (let [direction (if (= (first command) \L) -1 1)
        amount (read-string (subs command 1))
        old-value (:value state)
        new-value (-> state
                      :value
                      (+ (* direction amount)))]
    (-> state
        (assoc :value new-value)
        (update :count #(+ % (rotations-through-zero old-value new-value))))))

(defn day-01b [input]
  (->> input
       split-lines
       (reduce update-state-comprehensive default-state)
       :count))
