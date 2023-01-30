(defn __ [number]
  (let [numerals {1000 [\M]
                  900 [\C \M]
                  500 [\D]
                  400 [\C \D]
                  100 [\C]
                  90 [\X \C]
                  50 [\L]
                  40 [\X \L]
                  10 [\X]
                  9 [\I \X]
                  5 [\V]
                  4 [\I \V]
                  1 [\I]}]
    (loop [remaining number
           numeral []]
      (if (= remaining 0)
        (apply str numeral)
        (let [possibilities (filter (fn [[value' _]] (<= value' remaining)) numerals)
              [value sequence]
              (first (reverse (sort-by first possibilities)))]
          (recur (- remaining value) (concat numeral sequence)))))))
