(defn __ [trump-suit]
  (fn [cards]
    (let [leading-suit (-> cards first :suit)

          value (fn [card]
                  (+ (cond
                       (= (card :suit) trump-suit) 40
                       (= (card :suit) leading-suit) 20
                       :else 0)
                     (card :rank)))]
      (apply max-key value cards))))

(let [notrump (__ nil)]
  (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                           {:suit :club :rank 9}]))
       (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                           {:suit :club :rank 10}]))))
(= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                       {:suit :club :rank 10}]))
(= {:suit :heart :rank 8}
   ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                 {:suit :diamond :rank 10} {:suit :heart :rank 4}]))
