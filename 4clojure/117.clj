(defn __ [maze]
  (let [height (count maze)
        width (count (first maze))]

    (letfn [(depth-first-search [initial explore accept?]
              (loop [unexplored (list initial)
                     explored #{}]
                (let [current (first unexplored)]
                  (cond
                    (explored current) (recur (rest unexplored) explored)
                    (accept? current) current
                    :else (recur (concat (explore current) (rest unexplored))
                                 (conj explored current))))))]

      (depth-first-search 1 #(list (inc %)) #(= % 6)))))

(__ ["#######"
     "#     #"
     "#  #  #"
     "#M # C#"
     "#######"])