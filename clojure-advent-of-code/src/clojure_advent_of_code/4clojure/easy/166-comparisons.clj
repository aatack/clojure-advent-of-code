(defn __ [less-than? left right]
  (cond
    (less-than? left right) :lt
    (less-than? right left) :gt
    :else :eq))

(= :gt (__ < 5 1))
(= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
(= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
(= :gt (__ > 0 2))
