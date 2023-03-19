(ns advent-of-code.solutions.day-16
  (:require [advent-of-code.utils :refer [graph-distance beam-search]]
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
    {:pressures (zipmap (map first active-valves)
                        (map second active-valves))
     :graph (zipmap (map first valves)
                    (map (fn [[_ _ tunnels]]
                           tunnels)
                         valves))}))

(defn evaluate-plan
  "Determine the amount of pressure relieved by a given plan.
   
   The plan is given as a vector of valve names, which will be 
   taken to be the order in which the valves with non-zero pressure
   are visited and opened."
  [state plan]
  (let [graph (state :graph)]
    (loop [pressure 0
           minutes duration
           valves plan
           current-valve "AA"]
      (if (or (empty? valves) (<= minutes 0))
        pressure
        (let [next-valve (first valves)
              distance (graph-distance graph current-valve next-valve)
              ; One more to open the valve
              remaining-minutes (max 0 (- minutes (inc distance)))]
          (recur (+ pressure (* (get-in state [:pressures next-valve])
                                remaining-minutes))
                 remaining-minutes
                 (apply vector (rest valves))
                 next-valve))))))

(defn explore-plan [plan]
  (for [i (range (count plan))
        j (range (inc i) (count plan))
        :let [left (nth plan i)
              right (nth plan j)]]
    (-> plan
        (assoc i right)
        (assoc j left))))

(defn day-16a [input]
  (let [state (parse-state input)]
    (beam-search
     (apply vector (keys (state :pressures)))
     explore-plan
     #(evaluate-plan state %)
     10
     100)))

(defn day-16b [input]
  (->> input parse-state))
