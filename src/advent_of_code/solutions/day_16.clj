(ns advent-of-code.solutions.day-16
  (:require [clojure.string :refer [split-lines replace split]]))

(defn parse-state
  "Parse the initial state of the problem."
  [input]
  (let [valves (map (fn [[valve pressure & tunnels]]
                     [valve (read-string pressure) (apply vector tunnels)])
                   (map #(split % #" ")
                        (-> input
                            (replace "," "")
                            (replace "Valve " "")
                            (replace "has flow rate=" "")
                            (replace "; tunnels lead to valves" "")
                            (replace "; tunnel leads to valve" "")
                            (split-lines))))]
    {:valve "AA"
   :open #{}
   :minutes 30
   :relieved 0
   :valves (zipmap (map first valves)
                   (map (fn [[_ pressure tunnels]]
                          {:pressure pressure
                           :tunnels tunnels})
                        valves))}))

(defn relieve-state
  "Relive pressure from all open valves."
  [state]
  (update state
          :relieved
          #(+ %
              (apply +
                     (map (fn [valve]
                            (get-in state [:valves valve :pressure]))
                          (state :open))))))

(defn day-16a [input]
  (->> input
       parse-state
       relieve-state))

(defn day-16b [input]
  (->> input))
