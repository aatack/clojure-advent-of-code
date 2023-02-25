(defn __ [buffer sequence]
  (if (integer? sequence)
    sequence
    (second (reduce (fn [[remaining acc] element]
                      (let [result (__ remaining element)
                            increment (apply + (flatten [result]))]
                        (if (> increment remaining)
                          (reduced [remaining acc])
                          [(- remaining increment) (conj acc result)])))
                    [buffer []]
                    sequence))))

(=  (__ 10 [1 2 [3 [4 5] 6] 7])
    '(1 2 (3 (4))))
(=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
    '(1 2 (3 (4 (5 (6 (7)))))))
(=  (__ 9 (range))
    '(0 1 2 3))
(=  (__ 1 [[[[[1]]]]])
    '(((((1))))))
(=  (__ 0 [1 2 [3 [4 5] 6] 7])
    '())
(=  (__ 0 [0 0 [0 [0]]])
    '(0 0 (0 (0))))
(=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
    '(-10 (1 (2 3 (4)))))
