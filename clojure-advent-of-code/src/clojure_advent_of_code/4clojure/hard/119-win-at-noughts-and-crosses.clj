(defn __ [player board]
  (letfn [(winner [board']
            (let [transpose #(apply map (cons vector %))
                  diagonal #(map (fn [i] (nth (nth % i) i)) (range (count %)))
                  groups (concat board'
                                 (transpose board')
                                 [(diagonal board')]
                                 [(diagonal (reverse board'))])]
              (first (->> groups
                          (map set)
                          (filter #(= (count %) 1))
                          (map first)
                          (remove #(= % :e))))))
          
          (place [x y]
                 (update board x #(if (= (nth % y) :e)
                                    (assoc % y player)
                                    %)))]
    
    (set (filter #(= player (winner (place (first %) (second %))))
                 (for [i [0 1 2]
                       j [0 1 2]]
                   [i j])))))

(= (__ :x [[:o :e :e]
           [:o :x :o]
           [:x :x :e]])
   #{[2 2] [0 1] [0 2]})
(= (__ :x [[:x :o :o]
           [:x :x :e]
           [:e :o :e]])
   #{[2 2] [1 2] [2 0]})
(= (__ :x [[:x :e :x]
           [:o :x :o]
           [:e :o :e]])
   #{[2 2] [0 1] [2 0]})
(= (__ :x [[:x :x :o]
           [:e :e :e]
           [:e :e :e]])
   #{})
(= (__ :o [[:x :x :o]
           [:o :e :o]
           [:x :e :e]])
   #{[2 2] [1 1]})
