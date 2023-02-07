(defn __ [buffer sequence]
  (if (integer? sequence)
    sequence
    (second (reduce (fn [[remaining acc] element]
                      (let [result (__ remaining element)
                            increment (apply + (flatten [result]))]
                        (if (> increment remaining)
                          (reduced [remaining acc])
                          [(- remaining increment) (conj acc result)])))
                    [buffer []]
                    sequence))))
