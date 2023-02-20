(defn __
  ([value]
   value)
  ([left operator right & rest]
   (apply __ (operator left right) rest)))

(= 7  (__ 2 + 5))
(= 42 (__ 38 + 48 - 2 / 2))
(= 8  (__ 10 / 2 - 1 * 2))
(= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
