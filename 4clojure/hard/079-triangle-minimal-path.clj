(def __
  (fn [triangle]
    (letfn [(paths [triangle]
              (if (= (count triangle) 1)
                triangle
                (let [top (first (first triangle))
                      lower (rest triangle)
                      left (map butlast lower)
                      right (map rest lower)]
                  (map #(cons top %)
                       (concat (paths left)
                               (paths right))))))]
      (apply min (map #(apply + %) (paths triangle))))))

(= (__ [[1]
        [2 4]
        [5 1 4]
        [2 3 4 5]])
   (+ 1 2 1 3)
   7)
(= (__ [[3]
        [2 4]
        [1 9 3]
        [9 9 2 4]
        [4 6 6 7 8]
        [5 7 3 5 1 4]])
   (+ 3 4 3 2 7 1)
   20)