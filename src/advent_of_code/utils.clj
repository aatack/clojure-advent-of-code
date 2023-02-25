(ns advent-of-code.utils
  (:require [clojure.string :as string]))

(defn load-input [problem]
  (slurp (str "src/advent_of_code/inputs/"
              (string/replace (name problem) "-" "_")
              ".txt")))
