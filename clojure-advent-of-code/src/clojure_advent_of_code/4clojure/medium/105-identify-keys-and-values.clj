(defn __ [sequence]
  (first (reduce (fn [[groups key] value]
                   (if (keyword? value)
                     [(assoc groups value []) value]
                     [(update groups key #(conj % value)) key]))
                 [{} nil]
                 sequence)))

(= {} (__ []))
(= {:a [1]} (__ [:a 1]))
(= {:a [1]
    :b [2]} (__ [:a 1, :b 2]))
(= {:a [1 2 3]
    :b []
    :c [4]} (__ [:a 1 2 3 :b :c 4]))
