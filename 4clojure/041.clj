(def __ (fn [sequence n]
          (for [[index value] (map-indexed vector sequence)
                :when (not= (mod (inc index) n) 0)]
            value)))

(__ [:a :b :c :d :e :f] 2)

(= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
(= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])
