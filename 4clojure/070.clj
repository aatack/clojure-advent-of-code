(require '[clojure.string :as str])

(def __
  (fn [string]
    (let [words
          (-> string
              (.replaceAll "[^A-Za-z ]" "")
              (str/split #" "))]
      (sort-by #(str/lower-case %) words))))

(= (__  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (__  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (__  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])
