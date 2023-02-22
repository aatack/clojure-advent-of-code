(defn __ [number]
  (if (= number 0)
    #{""}
    (set (apply concat (for [combination (__ (dec number))]
                         [(str "()" combination)
                          (str "(" combination ")")
                          (str combination "()")])))))

((__ 9) "(((()()()())(())))")
(= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))
(= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
(= 16796 (count (__ 10)))
(= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))")
(= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))")
