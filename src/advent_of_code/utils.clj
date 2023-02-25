(ns advent-of-code.utils
  (:require [clojure.string :as string]))

(defmacro defsolution [problem & body]
  (let [problem-name (name problem)]
    (list 'defn problem []
        (apply list 'let
               ;; TODO: this currently loads the string in advance and
               ;;       includes it in the function as a literal, which is
               ;;       unpalatable
               ['input (slurp (str "src/advent_of_code/inputs/"
                                   (string/replace problem-name "-" "_")
                                   ".txt"))]
               body))))
