(ns advent-of-code-2023.solutions.day-20
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.graphs :refer [build-graph node-inputs
                                                node-subgraph]]
            [advent-of-code-2023.utils :refer [find-cycle map-vals
                                               split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-module [module]
  (let [[name children] (split-string " -> " module)
        broadcaster? (= name "broadcaster")
        children (split-string ", " children)]
    [(if broadcaster? name (subs name 1))
     {:outputs children
      :type (cond
              broadcaster? :broadcaster
              (= (first name) \%) :flip-flop
              :else :conjunction)}]))

(defn parse-modules [input]
  (let [modules (->> input split-lines (map parse-module)
                     (cons ["button" {:outputs ["broadcaster"] :type :broadcaster}]))]
    (reduce (fn [graph [name {:keys [type]}]]
              (assoc-in graph
                        [name :state]
                        (case type
                          :broadcaster nil
                          :flip-flop false
                          :conjunction (->> (get-in graph [name :inputs])
                                            (map #(vector % :low))
                                            (into {})))))
            (->> (for [[input {:keys [outputs]}] modules
                       output outputs]
                   [input output])
                 build-graph)
            modules)))

(defn fire [source {:keys [state]} strength]
  (cond
    (boolean? state) (if (= strength :high)
                       [state nil]
                       [(not state) (if state :low :high)])
    (map? state) (let [new-state (assoc state source strength)]
                   (if (= #{:high} (set (vals new-state)))
                     [new-state :low]
                     [new-state :high]))
    :else [state strength]))

(defn send-signals [initial-modules initial-sources]
  (loop [signals []
         sources initial-sources
         modules initial-modules
         pulses {:low 0 :high 0}]
    (if (empty? signals)
      (if (empty? sources)
        pulses
        (recur [(first sources)]
               (rest sources)
               modules
               pulses))
      (let [[source input-strength destination] (first signals)
            [state output-strength] (fire source (modules destination) input-strength)]
        (recur (apply conj (into [] (rest signals))
                      (if (nil? output-strength)
                        []
                        (map (fn [child] [destination output-strength child])
                             (get-in modules [destination :outputs]))))
               sources
               (assoc-in modules [destination :state] state)
               (update pulses input-strength inc))))))

(defn day-20a [input]
  (->> (send-signals (->> input parse-modules)
                     (repeat 1000 ["button" :low "broadcaster"]))
       vals
       (apply *)))

(defn press-button
  ([initial-modules] (press-button initial-modules #{}))
  ([initial-modules watch]
   (loop [signals [["button" :low "broadcaster"]]
          modules initial-modules
          step 0
          results {}]
     (if (empty? signals)
       (with-meta modules {:results results})
       (let [[source input-strength destination] (first signals)
             [state output-strength] (fire source (modules destination) input-strength)]
         (recur (apply conj (into [] (rest signals))
                       (if (nil? output-strength)
                         []
                         (map (fn [child] [destination output-strength child])
                              (get-in modules [destination :outputs]))))
                (assoc-in modules [destination :state] state)
                (inc step)
                (if (watch source)
                  (update results source #(conj (or % []) [step input-strength]))
                  results)))))))

(defn day-20b
  "Determine the solution to the second part.
   
   This works by identifying subgraphs with no circular dependencies.  If a subgraph has
   no circular dependencies, it follows that - given an unchanging source of inputs from
   the button - the behaviour of that subgraph must be cyclical (from the pidgeonhole
   principle: there are only a certain number of states that the subgraph can be in, so
   they must eventually repeat).
   
   First we identify all subgraphs with no circular dependencies, then we determine the
   cycle duration for each one.  **We then make the assumption that each of these nodes
   feeds into a single conjunction node, which then feeds out into the `rx` node.**
   Therefore we can treat each node's last input as its current input, and need only
   find the period of time for which each subgraph node is outputting a high signal
   strength.  Then we find a time where each of them are outputting a high signal
   strength simultaneously."
  [input]
  (let [graph (parse-modules input)
        subgraph-nodes (disj (->> graph
                                  (map #(vector (first %)
                                                (node-inputs graph (first %))))
                                  (filter second)
                                  (map first)
                                  set)
                             "button"
                             "broadcaster"
                             "rx"
                             (-> (graph "rx") :inputs first))
        history (doall (->> graph
                            (iterate #(press-button % subgraph-nodes))
                            (take 5000)))]
    (->> (for [node subgraph-nodes]
           [node (->> history
                      (map #(node-subgraph % node))
                      find-cycle
                      (#(select-keys % [:first :second])))])
         (into {}))))
