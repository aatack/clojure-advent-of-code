(defn custom-flatten [sequence]
  (if (or (vector? sequence) (list? sequence))
    (apply concat (map custom-flatten sequence))
    [sequence]))

(def __ custom-flatten)

(= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
(= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
(= (__ '((((:a))))) '(:a))
