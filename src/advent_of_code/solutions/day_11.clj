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
                                      "*" '*'} operator)
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
                    read-string)
     :inspected 0}))

(defn parse-monkeys [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map parse-monkey)
       (apply vector)))

(defn process-monkey [monkey-index monkeys worry-factor]
  (let [initial-monkey (nth monkeys monkey-index)
        worry (-> initial-monkey
                  :items
                  first
                  ((initial-monkey :operation))
                  (quot worry-factor))
        final-monkey (if (= 0 (mod worry (initial-monkey :condition)))
                       (initial-monkey :then)
                       (initial-monkey :otherwise))]
    (-> monkeys
        (update-in [monkey-index :items] rest)
        (update-in [monkey-index :inspected] inc)
        (update-in [final-monkey :items] #(concat % [worry])))))

(defn perform-round [initial-monkeys worry-factor]
  (loop [monkey-index 0
         monkeys initial-monkeys]
    (cond
      (>= monkey-index (count initial-monkeys))
      monkeys
      (empty? (get-in monkeys [monkey-index :items]))
      (recur (inc monkey-index) monkeys)
      :else
      (recur monkey-index (process-monkey monkey-index monkeys worry-factor)))))

(defn day-11a [input]
  (->> input
       parse-monkeys
       (iterate #(perform-round % 3))
       (drop 20)
       first
       (map :inspected)
       sort
       reverse
       (take 2)
       (apply *)))

(defn day-11b [input]
  (->> input
       parse-monkeys
       (iterate #(perform-round % 1))
       (drop 10000)
       first
       (map :inspected)
       sort
       reverse
       (take 2)
       (apply *)))
