(ns advent-of-code.solutions.day-03 
  (:require [clojure.string :refer [split-lines upper-case]]
            [clojure.set :refer [intersection]]))

(defn compartments [rucksack]
  (let [compartment-size (/ (count rucksack) 2)]
    [(set (take compartment-size rucksack))
     (set (drop compartment-size rucksack))]))

(defn shared-items [[left-compartment right-comparment]]
  (intersection left-compartment right-comparment))

(defn priority [item]
  (if (= item (first (upper-case item)))
    (+ 27 (- (int item) (int \A)))
    (+ 1 (- (int item) (int \a)))))

(defn day-03a [input]
  input)

(defn day-03b [input]
  input)
