(ns advent-of-code.solutions.day-11
  (:require [clojure.string :refer [split-lines split]]))

(def divisors [11 2 5 7 17 19 3 13])

(defn parse-modulos
  "Store a number as its modulos of each of the divisors.
   
   The number will be represented as a map of the form `{k j}`,
   where `k` is a number from `divisors` and `j` is an integer
   strictly less than `k`.  This denotes that there is some
   integer `i` for which `ik = j` is true."
  [number]
  (zipmap divisors (map #(mod number %) divisors)))

(defn add
  "Build a function to add a constant to a number's modulos.
   
   Each modulo, stored in the form ik + j (where `i` is
   unknown) has n added to it, giving ikn + j + n.  The first
   term is always divisible by k, so can be ignored; the second
   and third terms will then be stored as (j + n) % k."
  [constant]
  (fn [modulos]
    (zipmap (keys modulos)
            (map (fn [[multiple offset]]
                   (mod (+ offset constant) multiple))
                 modulos))))

(defn multiply
  "Build a function to multiply a number's modulos by a constant.
   
   Each modulo, stored in the form ik + j (where `i` is
   unknown) is multiplied by n, giving ikn + jn.  The first
   term is always divisible by k, so can be ignored; the second
   term will then be stored as jn % k."
  [constant]
  (fn [modulos]
    (zipmap (keys modulos)
            (map (fn [[multiple offset]]
                   (mod (* offset constant) multiple))
                 modulos))))

(defn square
  "Build a function to square a number's modulos.
   
   Each modulo, stored in the form ik + j (where `i` is
   unknown) is multiplied by itself, giving:
   
     (ik)^2 + 2ik + j^2
   
   Hence the first two terms are divisible by k and can be
   ignored; the final term can be stored as j^2 % k."
  []
  (fn [modulos]
    (zipmap (keys modulos)
            (map (fn [[multiple offset]]
                   (mod (* offset offset) multiple))
                 modulos))))

(defn divisible
  "Build a function for determining whether modulos are divisible."
  [constant]
  (fn [modulos]
    (= (modulos constant) 0)))

(defn parse-monkey [monkey]
  (let [[_ items operation condition then otherwise] monkey]
    {:items (map (comp parse-modulos read-string)
                 (-> items
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
       parse-monkeys))
