(ns advent-of-code.solutions.day-16
  (:require [advent-of-code.utils :refer [graph-distance beam-search]]
            [clojure.string :refer [replace split split-lines]]))

(def sole-duration 30)
(def dual-duration 26)

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

(defn plan-duration [state plan]
  (apply + -1 (map (fn [[start end]]
                     (inc (graph-distance (state :graph) start end)))
                   (partition 2 1 (cons "AA" plan)))))

(defn evaluate-plan
  "Determine the amount of pressure relieved by a given plan.
   
   The plan is given as a vector of valve names, which will be 
   taken to be the order in which the valves with non-zero pressure
   are visited and opened."
  [state duration]
  (fn [plan]
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
                   next-valve)))))))

(defn explore-plan
  "Produce a function for exploring plans similar to the given plan."
  [state duration]
  (let [valves (set (keys (state :pressures)))]
    (fn [plan]
      (let [options (apply disj valves plan)]
        (if (> (plan-duration state plan) duration)
          []
          (concat
           ;; Append a new step from the available options
           (for [option options]
             (conj plan option))
           ;; Swap any two visits in the current plan
           (for [i (range (count plan))
                 j (range (inc i) (count plan))
                 :let [left (nth plan i)
                       right (nth plan j)]]
             (-> plan
                 (assoc i right)
                 (assoc j left)))
           ;; Swap an existing visit for one of the options
           (for [i (range (count plan))
                 replacement options]
             (assoc plan i replacement))))))))

(defn day-16a [input]
  (let [state (parse-state input)]
    (first (beam-search
            []
            (explore-plan state sole-duration)
            (evaluate-plan state sole-duration)
            200
            200))))

(defn evaluate-dual-plan [state duration]
  (let [inner-evaluate (evaluate-plan state duration)]
    (fn [plan]
      (+ (inner-evaluate (plan :elf)) (inner-evaluate (plan :elephant))))))

(defn explore-dual-plan-branch
  "Produce a function for exploring dual plans similar to the one given."
  [state]
  (let [valves (set (keys (state :pressures)))]
    (fn [plan primary secondary]
      (let [options (apply disj valves (-> plan vals flatten))
            primary-plan (plan primary)
            secondary-plan (plan secondary)]
        (if (> (plan-duration state primary-plan) dual-duration)
          []
          (concat
           ;; Append a new step from the available options
           (for [option options]
             (update plan primary #(conj % option)))))))))

(defn explore-dual-plan
  [state]
  (let [inner-explore (explore-dual-plan-branch state)]
    (fn [plan]
      (concat (inner-explore plan :elf :elephant)
              (inner-explore plan :elephant :elf)))))

(defn day-16b [input]
  (let [state (parse-state input)]
    (beam-search
     {:elf [] :elephant []}
     (explore-dual-plan state)
     (evaluate-dual-plan state dual-duration)
     200
     200)))
