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
                    'D 'd} value)))

          (implies [left right]
            (every? (fn [condition]
                      (or (right condition)
                          (not (right ({'a 'A
                                        'A 'a
                                        'b 'B
                                        'B 'b
                                        'c 'C
                                        'C 'c
                                        'd 'D
                                        'D 'd} condition)))))
                    left))

          (check [input output]
            (every? (fn [input']
                      (some #(implies input' %) output))
                    input))

          (k-combinations [length items]
            (case length
              0 #{}
              1 (set (map #(set [%]) items))
              (set (for [outer (k-combinations (dec length) items)
                         inner items
                         :when (not (outer inner))]
                     (conj outer inner)))))

          (subsets [input-set]
            (apply concat (map #(k-combinations % input-set)
                               (-> input-set count inc range))))]

    (-> #{#{'a 'B 'C 'd}
          #{'A 'b 'c 'd}
          #{'A 'b 'c 'D}
          #{'A 'b 'C 'd}
          #{'A 'b 'C 'D}
          #{'A 'B 'c 'd}
          #{'A 'B 'c 'D 1}
          #{'A 'B 'C 'd 2}
          #{'A 'b 'c 'D 3}
          #{'A 'b 'C 'd 4}
          #{'A 'b 'C 'D 5}
          #{'A 'B 'c 'd 6}
          #{'A 'B 'c 'D 7}
          #{'A 'B 'C 'd 8}}
        subsets
        count)

    #_(let [candidates (for [function-subset function
                             subset-value function-subset
                             :let [comparison (swap-value function-subset
                                                          subset-value)
                                   new-subset (disj function-subset
                                                    subset-value)]
                             :when (and (not (function new-subset))
                                        (function comparison))]
                         (with-meta new-subset {:parents [function-subset comparison]}))]

        (if (empty? candidates)
          function ; TODO: remove the parents of each of the values
          (__ (apply conj function candidates))))))

(__ nil)

(= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c}
     #{'A 'b}
     #{'B 'C 'd}})

(= (__ #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'B 'C}})

(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}})
   #{#{'a 'c}
     #{'A 'C}})

(= (__ #{#{'a 'b 'c}
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})
   #{#{'a}})

(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}})

(= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}})
   #{#{'a 'c}
     #{'B 'c}})

(= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}})
   #{#{'B 'd}
     #{'b 'D}})

(= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}})

(= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}})

; Every one of the outputs must imply at least one of the inputs

aBCd
Abcd <-
AbcD
AbCd <-
AbCD
ABcd <-
ABcD
ABCd <-

Abc
AbC -- Ab Ac
ABc

BCd




aBCd
Abcd
AbcD
AbCd
AbCD
ABcd
ABcD
ABCd
BCd
Abc
