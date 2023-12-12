(ns advent-of-code-2023.utils
  (:require [clojure.string :as string :refer [split]]))

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
