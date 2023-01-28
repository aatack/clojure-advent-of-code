(defn __ [numeral]
  (let [numerals {\I 1
                  \V 5
                  \X 10
                  \L 50
                  \C 100
                  \D 500
                  \M 1000}]
    (let [pairs (partition 2 1 (concat [0] (map numerals numeral) [0]))]
      (reduce (fn [total [left right]]
                ((if (>= left right) + -) total left))
              0
              pairs))))
