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
                           (vector (concat [1] digits))
                           (assoc digits 0 1))
                 :even (not (palindrome :even))}
                (assoc palindrome :digits digits))))]
    (increment {:digits [9 9] :even false})))

(__ 0)

99 false
10 true
999 1001
10001

odd->even
999 -- 99
1001 -- 10

even->odd
99 -- 9
101 -- 10
