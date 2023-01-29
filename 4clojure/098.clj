(defn __ [function domain]
  (set (map #(set (second %))
            (reduce (fn [groups value]
                      (update groups (function value) #(conj (or % ()) value)))
                    {}
                    domain))))


(__ #(* % %) #{-2 -1 0 1 2})
