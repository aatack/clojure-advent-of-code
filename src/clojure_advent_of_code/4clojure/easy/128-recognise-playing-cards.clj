(defn __ [card]
  {:suit ({\H :heart
           \D :diamond
           \C :club
           \S :space} (first card))
   :rank ({\2 0
           \3 1
           \4 2
           \5 3
           \6 4
           \7 5
           \8 6
           \9 7
           \T 8
           \J 9
           \Q 10
           \K 11
           \A 12} (second card))})

(= {:suit :diamond :rank 10} (__ "DQ"))
(= {:suit :heart :rank 3} (__ "H5"))
(= {:suit :club :rank 12} (__ "CA"))
(= (range 13) (map (comp :rank __ str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))
