(defn __ [sequence]
  (letfn [(sequence? [value] (or (list? value) (vector? value)))
          (bottom-level? [inner-sequence]
            (and (sequence? inner-sequence)
                 (every? #(not (sequence? %)) inner-sequence)))]
    (cond
      (bottom-level? sequence) [sequence]
      (sequence? sequence) (apply concat (map __ sequence))
      :else [[sequence]])))

(= (__ [["Do"] ["Nothing"]])
   [["Do"] ["Nothing"]])
(= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
   [[:a :b] [:c :d] [:e :f]])
(= (__ '((1 2) ((3 4) ((((5 6)))))))
   '((1 2) (3 4) (5 6)))
