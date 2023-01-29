(defn __ [board]
  (let [alive-cells (set (for [[row-index row] (map-indexed vector board)
                               [column-index cell] (map-indexed vector row)
                               :when (= (str cell) "#")]
                           [row-index column-index]))
        next-iteration (fn [x y]
                         (let [neighbours (count (filter
                                                  alive-cells
                                                  (for [i [(dec x) x (inc x)]
                                                        j [(dec y) y (inc y)]
                                                        :when (not= [i j] [x y])] [i j])))
                               alive (alive-cells [x y])]
                           (if (or (and alive (#{2 3} neighbours))
                                   (and (not alive) (= neighbours 3)))
                             "#"
                             " ")))]
    (for [row (range (count board))]
      (apply str (for [column (range (count (first board)))]
                   (next-iteration row column))))))

(= (__ ["      "
        " ##   "
        " ##   "
        "   ## "
        "   ## "
        "      "])
   ["      "
    " ##   "
    " #    "
    "    # "
    "   ## "
    "      "])
