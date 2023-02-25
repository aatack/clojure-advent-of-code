(defn pack
  ([sequence] (pack sequence [] []))
  ([sequence finalised packed]
   (cond
     (empty? sequence) (if (empty? packed)
                         finalised
                         (conj finalised packed))
     (or (empty? packed) (= (last packed) (first sequence)))
     (recur (rest sequence) finalised (conj packed (first sequence)))
     :else (recur (rest sequence) (conj finalised packed) [(first sequence)]))))

(def __ pack)

(= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
