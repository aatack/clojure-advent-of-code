(defn __ [number]
  (let [numerals {1000 [\M]
                  500 [\D]
                  100 [\C]
                  50 [\L]
                  10 [\X]
                  5 [\V]
                  1 [\I]}]
    (loop [remaining number
           numeral []]
      (if (= remaining 0)
        (apply str numeral)
        (let [[value sequence]
              (first (filter (fn [[value' _]] (<= value' remaining)) numerals))]
          (recur (- remaining value) (concat numeral sequence)))))))

(__ 827)
