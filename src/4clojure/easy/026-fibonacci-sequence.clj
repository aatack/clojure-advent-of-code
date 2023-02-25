(defn fibonacci
  ([remaining] (fibonacci remaining []))
  ([remaining sequence]
   (let [length (count sequence)]
     (cond
       (= remaining 0) sequence
       (< length 2) (fibonacci (dec remaining) (conj sequence 1))
       :else (fibonacci
              (dec remaining)
              (conj sequence (+ (last sequence) (nth sequence (- length 2)))))))))

(def __ fibonacci)

(= (__ 3) '(1 1 2))
(= (__ 6) '(1 1 2 3 5 8))
(= (__ 8) '(1 1 2 3 5 8 13 21))
