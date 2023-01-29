(defn __ [number]
  (let [numerals {\M 1000
                  \D 500
                  \C 100
                  \L 50
                  \X 10
                  \V 5
                  \I 1}]
    (loop [remaining number
           numeral []]
      (if (= remaining 0)
        (apply str numeral)
        (let [[largest-numeral largest-value]
              (first (filter (fn [[_ n]] (<= n remaining)) numerals))]
          (recur (- remaining largest-value) (conj numeral largest-numeral)))))))
