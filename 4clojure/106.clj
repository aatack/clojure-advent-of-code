(defn __ [start end]
  (letfn [(beam-search [initial explore evaluate space limit]
            (loop [queued-nodes [initial]
                   attempts 0
                   seen #{}]
              (let [node (first queued-nodes)
                    nodes (rest queued-nodes)
                    explored (set (concat nodes (explore node)))]
                (cond
                  (= 0 (evaluate node)) attempts
                  (> attempts limit) nil
                  :else (recur (take space (sort-by evaluate
                                                    (apply disj explored seen)))
                               (inc attempts)
                               (conj seen node))))))

          (explore [value]
            (concat [(* value 2) (+ value 2)]
                    (if (even? value) [(/ value 2)] [])))
          (evaluate [value]
            (if (= value end)
              0
              (+ (abs (- value end))
                 (if (even? value) 0.5 0.0))))]

    ;; For some reason we count finding the target as an operation,
    ;; so we must count one more move than is actually made
    (let [length (beam-search start explore evaluate 100 10)]
      (when length (inc length)))))
