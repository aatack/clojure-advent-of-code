(ns advent-of-code-2025.helpers
  (:require
   [clojure.string :as string]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code_2025/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))

(defn integers-between [start end]
  (if (= start end)
    (list end)
    (lazy-seq (cons start (integers-between
                           (if (> end start)
                             (inc start)
                             (dec start)) end)))))
