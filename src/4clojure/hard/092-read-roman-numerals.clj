(defn __ [numeral]
  (let [numerals {\I 1
                  \V 5
                  \X 10
                  \L 50
                  \C 100
                  \D 500
                  \M 1000}
        pairs (partition 2 1 (concat (map numerals numeral) [0]))]
    (reduce (fn [total [left right]]
              ((if (>= left right) + -) total left))
            0
            pairs)))

(= 14 (__ "XIV"))
(= 827 (__ "DCCCXXVII"))
(= 3999 (__ "MMMCMXCIX"))
(= 48 (__ "XLVIII"))
