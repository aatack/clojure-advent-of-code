(defn __ [expression]
  (fn [values]
    (eval (list 'let
                (into [] (apply concat (map identity values)))
                expression))))
