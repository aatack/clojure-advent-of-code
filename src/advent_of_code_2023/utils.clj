(ns advent-of-code-2023.utils
  (:require [clojure.string :as string]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code_2023/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))
