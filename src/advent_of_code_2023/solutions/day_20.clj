(ns advent-of-code-2023.solutions.day-20
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [split-lines]]))

(defn parse-module [module]
  (let [[name children] (split-string " -> " module)
        broadcaster? (= name "broadcaster")]
    [(if broadcaster? name (subs name 1))
     {:outputs (split-string ", " children)
      :state (cond
               broadcaster? nil
               (= (first name) \%) :off
               :else {})}]))

(defn parse-modules [input]
  (assoc (->> input
              split-lines
              (map parse-module)
              (into {}))
         "button"
         {:state nil :outputs '("broadcaster")}))

(defn fire [module strength]
  [(:state module) strength])

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
      (let [[source input-strength] (first signals)
            [state output-strength] (fire (modules source) input-strength)]
        (recur (apply conj (rest signals) (map (fn [child] [child output-strength])
                                               (get-in modules [source :children])))
               sources
               (assoc-in modules [source :state] state)
               (update pulses input-strength inc))))))

(defn day-20a [input]
  (->> input
       parse-modules))

(defn day-20b [input]
  (->> input))
