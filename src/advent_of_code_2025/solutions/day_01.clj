(ns advent-of-code-2025.solutions.day-01
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [clojure.string :refer [split-lines]]))

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

(defn day-01b [input]
  (->> input))

(comment

  (day-01a "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"))
