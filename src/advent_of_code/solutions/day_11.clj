(ns advent-of-code.solutions.day-11
  (:require [clojure.string :refer [split-lines split]]))

(defn parse-monkey [monkey]
  (let [[_ items operation condition then otherwise] monkey]
    {:items (map read-string (-> items
                                 (split #": ")
                                 second
                                 (split #", ")))
     :operation (let [[left operator right]
                      (-> operation
                          (split #" = ")
                          second
                          (split #" "))]
                  (eval (list 'fn '[old]
                              (list ({"+" '+
                                      "-" '-
                                      "/" '/
                                      "*" '*} operator)
                                    (if (= left "old")
                                      'old
                                      (read-string left))
                                    (if (= right "old")
                                      'old
                                      (read-string right))))))
     :condition (-> condition
                    (split #" ")
                    last
                    read-string)
     :then (-> then
               (split #" ")
               last
               read-string)
     :otherwise (-> otherwise
                    (split #" ")
                    last
                    read-string)}))

(defn parse-monkeys [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map parse-monkey)))

(defn day-11a [input]
  (->> input
       parse-monkeys))

(defn day-11b [input]
  (->> input))
