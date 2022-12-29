(def __
  (fn [sequence]
    (let [left-a (gensym)
          right-a (gensym)
          left-b (gensym)
          right-b (gensym)
          updated (conj (conj sequence [left-a right-a])
                        [left-b right-b])]
      (cond
        (= right-a (get updated left-a)) :map
        (= [left-a right-a] (get updated [left-a right-a])) :set
        (= [left-b right-b] (first updated)) :list
        (= [left-b right-b] (last updated)) :vector))))

(= :map (__ {:a 1, :b 2}))
(= :list (__ (range (rand-int 20))))
(= :vector (__ [1 2 3 4 5 6]))
(= :set (__ #{10 (rand-int 5)}))
(= [:map :set :vector :list] (map __ [{} #{} [] ()]))
