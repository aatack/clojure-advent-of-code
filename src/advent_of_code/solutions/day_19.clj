(ns advent-of-code.solutions.day-19
  (:require [clojure.string :refer [split-lines replace split]]))

(def duration 24)

(def empty-inventory {:ore 0
                      :clay 0
                      :obsidian 0
                      :geode 0})

(defn initial-state [blueprint]
  {:time duration
   :inventory empty-inventory
   :robots (update empty-inventory :ore inc)
   :blueprint blueprint})

(defn parse-cost [input]
  (->> (split input #",")
       (map #(split % #" "))
       (map (fn [[quantity material]]
              [(read-string (str ":" material))
               (read-string quantity)]))
       (into empty-inventory)))

(defn parse-blueprint [input]
  (let [[id ore clay obsidian geode]
        (-> input
            (replace "Blueprint " "")
            (replace ": Each ore robot costs " ";")
            (replace ". Each clay robot costs " ";")
            (replace ". Each obsidian robot costs " ";")
            (replace ". Each geode robot costs " ";")
            (replace " and " ",")
            (replace "." "")
            (split #";"))]
    {:id (read-string id)
     :ore (parse-cost ore)
     :clay (parse-cost clay)
     :obsidian (parse-cost obsidian)
     :geode (parse-cost geode)}))

(defn parse-blueprints [input]
  (->> input
       split-lines
       (map parse-blueprint)))

(defn step
  "Step the simulation forward by one minute, building robots where possible.
   
   This first looks at whether the next robot in the plan can be built.  If it can,
   construction is started, and the resources are removed.  Then the resources from the
   existing robots are added.  If a robot was built at the start, it is added to the
   list of existing robots at the end.
   
   Returns the next state and any remaining steps of the plan."
  [[plan state]]
  (let [robot (first plan)
        cost (get-in state [:blueprint robot])
        build? (and robot
                    (every? #(>= (get-in state [:inventory %]) (cost %))
                            (keys cost)))
        inventory (into {} (map (fn [material]
                                  [material
                                   (- (+ (get-in state [:inventory material])
                                         (get-in state [:robots material]))
                                      (if build?
                                        (cost material)
                                        0))])
                                (keys (state :inventory))))]
    [(if build? (rest plan) plan)
     (let [next-state (-> state
                          (assoc :inventory inventory)

                          (update :time dec))]
       (if build?
         (update-in next-state [:robots robot] inc)
         next-state))]))

(defn propagate [state]
  (fn [plan]
    (->> (iterate step [plan state])
         (drop-while #(> (get-in % [1 :time]) 0))
         first)))

(defn day-19a [input]
  ((->> input
        parse-blueprints
        first
        initial-state
        propagate) [:clay :clay :clay :obsidian :clay :obsidian :geode :geode]))

(defn day-19b [input]
  (->> input))
