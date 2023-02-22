(def __
  (fn [words]
    (->> words
         (reduce (fn [groups word]
                   (let
                    ;; NOTE: this may not work properly for words
                    ;;       with repeated letters
                    [group (set word)]
                     (update groups
                             group
                             #(conj (or % #{}) word))))
                 {})
         (map second)
         (filter #(> (count %) 1))
         set)))

(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})
(= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
