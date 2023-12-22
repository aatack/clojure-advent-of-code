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
               :else (zipmap children (repeat :low)))}]))

(defn parse-modules [input]
  (assoc (->> input
              split-lines
              (map parse-module)
              (into {}))
         "button"
         {:state nil :outputs '("broadcaster")}))

(defn fire [source {:keys [state]} strength]
  (cond
    (keyword? state) (if state
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
      (let [[source input-strength] (first signals)
            [state output-strength] (fire source (modules source) input-strength)]
        (recur (apply conj (rest signals)
                      (if (nil? output-strength)
                        []
                        (map (fn [child] [child output-strength])
                             (get-in modules [source :children]))))
               sources
               (assoc-in modules [source :state] state)
               (update pulses input-strength inc))))))

(defn day-20a [input]
  (send-signals (->> input
                     parse-modules) [["button" :low]]))

(defn day-20b [input]
  (->> input))
