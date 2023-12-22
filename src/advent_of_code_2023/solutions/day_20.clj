(ns advent-of-code-2023.solutions.day-20
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-module [module]
  (let [[name children] (split-string " -> " module)
        broadcaster? (= name "broadcaster")
        children (split-string ", " children)]
    [(if broadcaster? name (subs name 1))
     {:outputs children
      :state (cond
               broadcaster? nil
               (= (first name) \%) false
               :else {})}]))

(defn populate-inputs [modules]
  (into {}
        (map (fn [[key value]]
               [key (if (map? (:state value))
                      (let [inputs (->> modules
                                        (filter #((-> % second :outputs set) key))
                                        (map first))]
                        (assoc value :state (zipmap inputs (repeat :low))))
                      value)])
             modules)))

(defn parse-modules [input]
  (populate-inputs (assoc (->> input
                               split-lines
                               (map parse-module)
                               (into {}))
                          "button"
                          {:state nil :outputs '("broadcaster")})))

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

(defn send-signals-until-rx [initial-modules]
  (loop [signals []
         modules initial-modules
         presses 0]
    (if (empty? signals)
      (recur [["button" :low "broadcaster"]]
             modules
             (inc presses))
      (let [[source input-strength destination] (first signals)
            [state output-strength] (fire source (modules destination) input-strength)]
        (if (and (= destination "rx") (= input-strength :low))
          presses
        (recur (apply conj (into [] (rest signals))
                      (if (nil? output-strength)
                        []
                        (map (fn [child] [destination output-strength child])
                             (get-in modules [destination :outputs]))))
               (assoc-in modules [destination :state] state)
               presses))))))

(defn day-20b [input]
  (->> (send-signals-until-rx (->> input parse-modules))))
