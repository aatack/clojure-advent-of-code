(defn __ [initial]
  (letfn [(number->digits [number]
            (map {\0 0
                  \1 1
                  \2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9} (str number)))
          (digits->number [digits]
            (->> digits (apply str) Integer/parseInt))

          (increment [palindrome]
            (let [[digits carry]
                  (reduce (fn [[digits carry] digit]
                            (let [new-digit (if carry (inc digit) digit)]
                              (if (= new-digit 10)
                                [(conj digits 0) true]
                                [(conj digits new-digit) false])))
                          [[] true]
                          (palindrome :digits))]
              (if carry
                {:digits (if (palindrome :even)
                           (apply vector (concat digits [1]))
                           (assoc digits 0 1))
                 :even (not (palindrome :even))}
                (assoc palindrome :digits digits))))

          (palindrome->number [palindrome]
            (-> (concat (reverse (palindrome :digits))
                        ((if (palindrome :even) identity rest)
                         (palindrome :digits)))
                digits->number))]

    (let [initial-digits (number->digits initial)
          initial-even? (even? (count initial-digits))
          initial-count (if initial-even?
                          (/ (count initial-digits) 2)
                          (/ (inc (count initial-digits)) 2))

          initial-palindrome
          (first (filter #(>= (palindrome->number %) initial)
                         (iterate increment
                                  {:digits (into [] (reverse (take initial-count
                                                          initial-digits)))
                                   :even initial-even?})))]

      (map palindrome->number
             (iterate increment initial-palindrome)))))

(= (take 26 (__ 0))
   [0 1 2 3 4 5 6 7 8 9
    11 22 33 44 55 66 77 88 99
    101 111 121 131 141 151 161])
