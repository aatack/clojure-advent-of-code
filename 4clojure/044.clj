(def __
  (fn [delta sequence]
    (let [length (count sequence)
          adjusted (mod delta length)]
      (concat (take-last (- length adjusted) sequence)
              (take adjusted sequence)))))

(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
(= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
(= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
(= (__ 1 '(:a :b :c)) '(:b :c :a))
(= (__ -4 '(:a :b :c)) '(:c :a :b))
