(ns advent-of-code-2023.solutions.day-20
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2022.utils :refer [enumerate]]
            [advent-of-code-2023.graphs :refer [build-graph node-inputs
                                                node-subgraph]]
            [advent-of-code-2023.solutions.day-08 :refer [lowest-common-multiple]]
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
   strength simultaneously.
   
   As it transpires, all nodes emit a high signal strength for an overlapping period
   within exactly one step in the cycle.  The full information from this can be seen in
   the `:activation` key of the output map.  Because the periods for which the nodes
   are emitting a high signal strength overlap within the button press, we need only
   determine when the button presses (ie. outer loops instead of inner loops) will
   overlap.
   
   It is assumed that this overlap period holds true for all puzzle inputs.  It is also
   assumed that, for all puzzle inputs, the button press within which the high-strength
   pulse occurs is in the `n - 1`th button press of every cycle, where `n` is the number
   of states in the cycle.
   
   Hence, also assuming that each cycle starts on the second button press, we need only
   find the lowest common multiple of the cycle lengths of the subgraph nodes.  (The
   cycles will all end at the same time on the button press whose number is the lowest
   common multiple plus one - because the cycles don't start until the second button
   press.  Then the high output occurs one press *before* the end of each cycle; the two
   cancel out.)"
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
           [node {:cycle (->> history
                              (map #(node-subgraph % node))
                              find-cycle
                              (#(select-keys % [:first :second])))
                  :activation (->> (for [[outer-index results]
                                         (->> history
                                              rest
                                              (map (comp #(get % node) :results meta))
                                              enumerate)
                                         [inner-index state] results]
                                     [[outer-index inner-index] state])
                                   (partition-by second)
                                   (map first))}])
         (into {})
         (map #(get-in % [1 :cycle]))
         (map (fn [{:keys [first second]}] (- second first)))
         (apply lowest-common-multiple))))
