(def __
  (fn [& booleans]
    (boolean (and (some identity booleans)
                  (not (every? identity booleans))))))

(= false (__ false false))
(= true (__ true false))
(= false (__ true))
(= true (__ false true false))
(= false (__ true true true))
(= true (__ true true true false))
