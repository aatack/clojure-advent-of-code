(def __
  (fn [board]
    (let [transpose #(apply map (cons vector %))
          diagonal #(map (fn [i] (nth (nth % i) i)) (range (count %)))
          groups (concat board
                         (transpose board) 
                         [(diagonal board)]
                         [(diagonal (reverse board))])]
      (first (->> groups
           (map set)
           (filter #(= (count %) 1))
           (map first)
           (remove #(= % :e))
           )))))

(= nil (__ [[:e :e :e]
            [:e :e :e]
            [:e :e :e]]))
(= :x (__ [[:x :e :o]
           [:x :e :e]
           [:x :e :o]]))
(= :o (__ [[:e :x :e]
           [:o :o :o]
           [:x :e :x]]))
(= nil (__ [[:x :e :o]
            [:x :x :e]
            [:o :x :o]]))
(= :x (__ [[:x :e :e]
           [:o :x :e]
           [:o :e :x]]))
(= :o (__ [[:x :e :o]
           [:x :o :e]
           [:o :e :x]]))
(= nil (__ [[:x :o :x]
            [:x :o :x]
            [:o :x :o]]))