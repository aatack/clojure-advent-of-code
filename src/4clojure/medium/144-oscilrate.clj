(defn __ [value & functions]
  (lazy-seq (cons value
                  (let [function (first functions)]
                    (apply __ (function value)
                           (concat (rest functions) [function]))))))

(= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
(= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
(= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
