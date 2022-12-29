(def __
  (fn [& sets]
    (set (filter #(every? (fn [s] (s %)) sets)
                 (apply concat sets)))))

(= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
(= (__ #{0 1 2} #{3 4 5}) #{})
(= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})
