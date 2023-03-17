(ns advent-of-code.solutions.day-16
  (:require [advent-of-code.utils :refer [beam-search]]
            [clojure.string :refer [replace split split-lines]]))

(def duration 30)

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
     :minutes duration
     :relieved 0
     :valves (zipmap (map first valves)
                     (map (fn [[_ pressure tunnels]]
                            {:pressure pressure
                             :tunnels tunnels})
                          valves))}))

(defn relief-rate [state]
  (apply +
         (map (fn [valve]
                (get-in state [:valves valve :pressure]))
              (state :open))))

(defn relieve
  "Relive pressure from all open valves."
  [state]
  (update state
          :relieved
          #(+ % (relief-rate state))))

(defn open
  "Open the current valve, incurring the corresponding time penalty."
  [state]
  (-> state
      (update :open #(conj % (state :valve)))
      (update :minutes dec)))

(defn move
  "Move to a valve, incurring the corresponding time penalty."
  [state valve]
  (-> state
      (assoc :valve valve)
      (update :minutes dec)))

(defn explore [state]
  (if (<= (state :minutes) 0)
    []
    (let [stepped (relieve state)
          valve (stepped :valve)]
      (concat [(open stepped)]
              (map #(move stepped %)
                   (-> stepped :valves (get valve) :tunnels))))))

(defn potential [state]
  (+ (state :relieved)
     (* (relief-rate state)
        (state :minutes))))

(defn day-16a [input]
  (->> (beam-search (parse-state input)
                    explore
                    potential
                    200)
       (sort-by potential)
       last))

(defn day-16b [input]
  (-> input
      parse-state
      (move "UO")
      open
      potential))
