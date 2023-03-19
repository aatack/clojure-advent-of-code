(ns advent-of-code.solutions.day-16
  (:require [advent-of-code.utils :refer [graph-distance]]
            [clojure.string :refer [replace split split-lines]]))

(def duration 30)

(defn parse-state
  "Parse the initial state of the problem."
  [input]
  (let [valves (map (fn [[valve pressure & tunnels]]
                      [valve (read-string pressure) (set tunnels)])
                    (map #(split % #" ")
                         (-> input
                             (replace "," "")
                             (replace "Valve " "")
                             (replace "has flow rate=" "")
                             (replace "; tunnels lead to valves" "")
                             (replace "; tunnel leads to valve" "")
                             (split-lines))))
        active-valves (filter (fn [[_ pressure _]]
                                (> pressure 0))
                              valves)]
    {:closed (zipmap (map first active-valves)
                     (map second active-valves))
     :graph (zipmap (map first valves)
                    (map (fn [[_ _ tunnels]]
                           tunnels)
                         valves))}))

(defn relief-rate [state]
  (apply +
         (map (fn [valve]
                (get-in state [:valves valve :pressure]))
              (state :open))))

(defn day-16a [input]
  (->> input parse-state))

(defn day-16b [input]
  (->> input parse-state))
