(def __ (class (class ())))

(let [x __]
  (and (= (class x) x) x))
