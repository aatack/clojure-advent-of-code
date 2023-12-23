(ns advent-of-code-2023.graphs)

(defn build-graph [input-output-pairs]
  (reduce (fn [graph [input output]]
            (-> graph
                (update input #(or % {:inputs #{} :outputs #{}}))
                (update output #(or % {:inputs #{} :outputs #{}}))
                (update-in [input :outputs] conj output)
                (update-in [output :inputs] conj input)))
          {}
          input-output-pairs))

(defn all-inputs [graph node]
  (loop [unexplored (set (get-in graph [node :inputs]))
         explored #{}]
    (if (empty? unexplored)
      explored
      (let [current (first unexplored)]
        (if (= current node)
          nil ;; There is a circular dependency
          (recur (apply conj
                        (disj unexplored current)
                        (->> (get-in graph [current :inputs])
                             (remove unexplored)
                             (remove explored)))
                 (conj explored current)))))))
