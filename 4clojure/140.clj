(defn __ [function]
  (letfn [(swap-value [subset value]
            (conj (disj subset value)
                  ({'a 'A
                    'A 'a
                    'b 'B
                    'B 'b
                    'c 'C
                    'C 'c
                    'd 'D
                    'D 'd} value)))]

    (let [candidates (for [function-subset function
                           subset-value function-subset]
                       [function-subset
                        (swap-value function-subset
                                    subset-value)])
          incumbants (filter #(function (second %)) candidates)]

      (if (empty? incumbants)
        function
        (let [selected (first incumbants)]
          (__ (conj (apply disj function selected)
                    (set (filter (first selected)
                                 (second selected))))))))))
