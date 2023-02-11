(defn __ [maze]
  (let [height (count maze)
        width (count (first maze))

        lookup (fn [x y]
                 (println x y)
                 (if (< -1 y height)
                   (if (< -1 x width)
                     (nth (nth maze y) x)
                     "#")
                   "#"))

        mouse (first (for [x (range 0 width)
                           y (range 0 height)
                           :when (= (lookup x y) \M)]
                       [x y]))

        cheese (first (for [x (range 0 width)
                            y (range 0 height)
                            :when (= (lookup x y) \C)]
                        [x y]))

        depth-first-search
        (fn [initial explore accept?]
          (loop [unexplored (list initial)
                 explored #{}]
            (let [current (first unexplored)]
              (cond
                (nil? current) false
                (accept? current) true
                (explored current) (recur (rest unexplored) explored)
                :else (recur (concat (explore current) (rest unexplored))
                             (conj explored current))))))]

    (boolean (depth-first-search
              mouse
              (fn [[x y]] (if (= (lookup x y) \#)
                            []
                            [[(dec x) y]
                             [(inc x) y]
                             [x (dec y)]
                             [x (inc y)]]))
              #(= % cheese)))))

(__ ["#######"
     "#     #"
     "#  #  #"
     "#M # C#"
     "#######"])