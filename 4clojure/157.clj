(defn __ [sequence]
  (map vector sequence (range)))

(= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
(= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
(= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])
