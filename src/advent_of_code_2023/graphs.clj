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
