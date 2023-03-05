(defn __ [left right]
  (map {\0 0
        \1 1
        \2 2
        \3 3
        \4 4
        \5 5
        \6 6
        \7 7
        \8 8
        \9 9} (str (* left right))))

(= (__ 1 1) [1])
(= (__ 99 9) [8 9 1])
(= (__ 999 99) [9 8 9 0 1])