(ns advent-of-code-2023.utils
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [clojure.string :as string :refer [split]]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code_2023/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))

(defn split-string [pattern string]
  (filter not-empty (split string (re-pattern pattern))))

(defn unique-pairs [items]
  (into #{} (for [left items
                  right items
                  :when (not= left right)]
              (set [left right]))))

(defn transpose-lines [lines]
  (apply map str lines))

(defn find-cycle [sequence]
  (reduce (fn [cache [index value]]
            (if (cache value)
              (reduced
               {:first (cache value) :second index
                :lookup (into {} (map (fn [[k v]] [v k]) cache))})
              (assoc cache value index)))
          {}
          (enumerate sequence)))

(defn lookup-cycle [index {:keys [first second lookup]}]
  (or (lookup index) (lookup (+ first (mod (- index second) (- second first))))))
