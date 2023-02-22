(defn __ [function]
  (fn [& arguments]
    (loop [current-function function
           current-arguments arguments]
      (if (empty? current-arguments)
        current-function
        (recur (current-function (first current-arguments))
               (rest current-arguments))))))

(= 10 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (+ a b c d))))))
       1 2 3 4))

(= 24 ((__ (fn [a]
             (fn [b]
               (fn [c]
                 (fn [d]
                   (* a b c d))))))
       1 2 3 4))

(= 25 ((__ (fn [a]
             (fn [b]
               (* a b))))
       5 5))
