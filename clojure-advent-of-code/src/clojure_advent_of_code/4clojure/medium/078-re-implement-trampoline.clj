(require 'clojure.test)

(defn custom-trampoline [function & arguments]
  (if (clojure.test/function? function)
    (custom-trampoline (apply function arguments))
    function))

(def __ custom-trampoline)

(= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
     (map (partial __ my-even?) (range 6)))
   [true false true false true false])
