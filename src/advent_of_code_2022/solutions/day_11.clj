(ns advent-of-code-2022.solutions.day-11
  (:require [clojure.string :refer [split-lines split]]))

;; TODO: come up with a nicer polymorphic solution for this

(def divisors [11 2 5 7 17 19 3 13 23])

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
  (fn [worry]
    (if (integer? worry)
      (+ worry constant)
      (zipmap (keys worry)
              (map (fn [[multiple offset]]
                     (mod (+ offset constant) multiple))
                   worry)))))

(defn multiply
  "Build a function to multiply a number's modulos by a constant.
   
   Each modulo, stored in the form ik + j (where `i` is
   unknown) is multiplied by n, giving ikn + jn.  The first
   term is always divisible by k, so can be ignored; the second
   term will then be stored as jn % k."
  [constant]
  (fn [worry]
    (if (integer? worry)
      (* worry constant)
      (zipmap (keys worry)
              (map (fn [[multiple offset]]
                     (mod (* offset constant) multiple))
                   worry)))))

(defn square
  "Build a function to square a number's modulos.
   
   Each modulo, stored in the form ik + j (where `i` is
   unknown) is multiplied by itself, giving:
   
     (ik)^2 + 2ik + j^2
   
   Hence the first two terms are divisible by k and can be
   ignored; the final term can be stored as j^2 % k."
  []
  (fn [worry]
    (if (integer? worry)
      (* worry worry)
      (zipmap (keys worry)
              (map (fn [[multiple offset]]
                     (mod (* offset offset) multiple))
                   worry)))))

(defn relieve
  "Reduce the worry level about an item.
   
   Does nothing to worry levels stored as modulos."
  [worry]
  (if (integer? worry)
    (quot worry 3)
    worry))

(defn divisible
  "Build a function for determining whether modulos are divisible."
  [constant]
  (fn [worry]
    (if (integer? worry)
      (= 0 (mod worry constant))
      (= (worry constant) 0))))

(defn parse-monkey [monkey integer-worries?]
  (let [[_ items operation condition then otherwise] monkey
        worries (map read-string (-> items
                                     (split #": ")
                                     second
                                     (split #", ")))]
    {:items (map (if integer-worries? identity parse-modulos) worries)
     :operation (let [[left operator right]
                      (-> operation
                          (split #" = ")
                          second
                          (split #" "))]
                  (cond
                    (= left right "old") (square)
                    (= operator "*") (multiply (read-string right))
                    :else (add (read-string right))))
     :condition (divisible (-> condition
                               (split #" ")
                               last
                               read-string))
     :then (-> then
               (split #" ")
               last
               read-string)
     :otherwise (-> otherwise
                    (split #" ")
                    last
                    read-string)
     :inspected 0}))

(defn parse-monkeys [integer-worries? input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)
       (map #(parse-monkey % integer-worries?))
       (apply vector)))

(defn process-monkey [monkey-index monkeys]
  (let [initial-monkey (nth monkeys monkey-index)
        worry (-> initial-monkey
                  :items
                  first
                  ((initial-monkey :operation))
                  relieve)
        final-monkey (if ((initial-monkey :condition) worry)
                       (initial-monkey :then)
                       (initial-monkey :otherwise))]
    (-> monkeys
        (update-in [monkey-index :items] rest)
        (update-in [monkey-index :inspected] inc)
        (update-in [final-monkey :items] #(concat % [worry])))))

(defn perform-round [initial-monkeys]
  (loop [monkey-index 0
         monkeys initial-monkeys]
    (cond
      (>= monkey-index (count initial-monkeys))
      monkeys
      (empty? (get-in monkeys [monkey-index :items]))
      (recur (inc monkey-index) monkeys)
      :else
      (recur monkey-index (process-monkey monkey-index monkeys)))))

(defn day-11a [input]
  (->> input
       (parse-monkeys true)
       (iterate perform-round)
       (drop 20)
       first
       (map :inspected)
       sort
       reverse
       (take 2)
       (apply *)))

(defn day-11b [input]
  (->> input
       (parse-monkeys false)
       (iterate perform-round)
       (drop 10000)
       first
       (map :inspected)
       sort
       reverse
       (take 2)
       (apply *)))
