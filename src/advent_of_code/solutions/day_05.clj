(ns advent-of-code.solutions.day-05
  (:require [clojure.string :refer [split-lines split]]))

(defn transpose [sequences]
  (apply map vector sequences))

(defn split-input
  "Splits input into stack and procedure sections."
  [input]
  (->> input
       split-lines
       (partition-by empty?)
       (take-nth 2)))

(defn parse-stack [stack]
  (->> stack
       butlast
       (map rest)
       (map #(take-nth 4 %))
       (map #(apply str %))
       transpose
       (map #(drop-while (fn [character] (= character \space)) %))
       (zipmap (rest (range)))))

(defn parse-procedure [procedure]
  (->> procedure
       (map #(split % #" "))
       (map rest)
       (map #(take-nth 2 %))
       (map #(map read-string %))
       (map (fn [[amount from to]]
              {:amount amount :from from :to to}))))

(defn make-apply-procedure [reversed?]
  (fn [stack procedure]
    (let [amount (procedure :amount)
          crates ((if reversed? reverse identity)
                  (take amount (stack (procedure :from))))]
      (-> stack
          (update (procedure :from) #(drop amount %))
          (update (procedure :to) #(concat crates %))))))

(defn apply-procedure [input reversed?]
  (let [[stack-input procedure-input] (split-input input)
        initial-stack (parse-stack stack-input)
        procedure (parse-procedure procedure-input)
        final-stack (reduce (make-apply-procedure reversed?)
                            initial-stack procedure)]
    (apply str (map #(first (final-stack %)) (sort (keys final-stack))))))

(defn day-05a [input]
  (apply-procedure input true))

(defn day-05b [input]
  (apply-procedure input false))
