(ns advent-of-code-2025.helpers
  (:require
   [clojure.string :as string]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code_2025/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))
