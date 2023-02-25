(defn custom-partition [length sequence]
  (if (< (count sequence) length)
    []
    (cons (take length sequence)
          (custom-partition length (drop length sequence)))))

(def __ custom-partition)

(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
