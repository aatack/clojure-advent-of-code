(def __
  (fn custom-iterate [function value]
    (lazy-seq (cons value
                    (custom-iterate function (function value))))))

(= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
(= (take 100 (__ inc 0)) (take 100 (range)))
(= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))
