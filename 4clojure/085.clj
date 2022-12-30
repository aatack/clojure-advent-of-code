(defn power-sets [initial-set]
  (if (empty? initial-set)
    #{#{}}
    (let [power-sets' (power-sets (rest initial-set))
          pivot (first initial-set)]
      (set (concat (map #(conj % pivot) power-sets')
                   power-sets')))))

(def __ power-sets)

(= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
(= (__ #{}) #{#{}})
(= (__ #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
(= (count (__ (into #{} (range 10)))) 1024)
