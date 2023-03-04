(ns advent-of-code.solutions.day-03 
  (:require [clojure.string :refer [split-lines]]
            [clojure.set :refer [intersection]]))

(defn compartments [rucksack]
  (let [compartment-size (/ (count rucksack) 2)]
    [(take compartment-size rucksack)
     (drop compartment-size rucksack)]))

(defn shared-items [left-compartment right-comparment]
  (intersection (vector left-compartment) (vector right-comparment)))

(defn day-03a [input]
  input)

(defn day-03b [input]
  input)
