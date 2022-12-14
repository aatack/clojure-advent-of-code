(def __
  (fn [value sequence]
    (butlast (apply concat (map vector
                                sequence
                                (repeat (count sequence) value))))))

(= (__ 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
(= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
