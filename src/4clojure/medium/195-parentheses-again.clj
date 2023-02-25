(def __
  (memoize (fn [number]
             (if (= number 0)
               #{""}
               (set
                (for [before (range number)
                      between (range (- number before))
                      :let [after (- (dec number) before between)]
                      before-combination (__ before)
                      between-combination (__ between)
                      after-combination (__ after)]
                  (str before-combination "("
                       between-combination ")"
                       after-combination)))))))

(= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (__ n)) [0 1 2]))
(= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (__ 3))
(= 16796 (count (__ 10)))
(= (nth (sort (filter #(.contains ^String % "(()()()())") (__ 9))) 6) "(((()()()())(())))")
(= (nth (sort (__ 12)) 5000) "(((((()()()()()))))(()))")
