(defn compress
  ([string] (compress string []))
  ([sequence compressed]
   (cond
     (empty? sequence) (if (string? sequence)
                         (apply str compressed)
                         compressed)
     (or (empty? compressed)
         (not= (last compressed) (first sequence)))
     (recur (rest sequence) (conj compressed (first sequence)))
     :else (recur (rest sequence) compressed))))

(def __ compress)

(= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
(= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
