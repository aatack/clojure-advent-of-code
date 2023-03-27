(ns advent-of-code.solutions.day-19
  (:require [advent-of-code.utils :refer [beam-search]]
            [clojure.string :refer [replace split split-lines]]))

(def duration 32) ; TODO: allow this to be changed per-part

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

(defn propagate [plan state]
  (->> (iterate step [plan state])
       (drop-while #(> (get-in % [1 :time]) 0))
       first))

(defn evaluate [state]
  (fn [plan]
    (let [[_ final-state] (propagate plan state)]
      (+ (* 0.01 (get-in final-state [:inventory :ore]))
         (* 0.2 (get-in final-state [:inventory :clay]))
         (* 2 (get-in final-state [:inventory :obsidian]))
         (* 20 (get-in final-state [:inventory :geode]))

         (* 1 (get-in final-state [:robots :ore]))
         (* 20 (get-in final-state [:robots :clay]))
         (* 200 (get-in final-state [:robots :obsidian]))
         (* 2000 (get-in final-state [:robots :geode]))))))

(defn score [state]
  (fn [plan]
    (let [[_ final-state] (propagate plan state)]
      (get-in final-state [:inventory :geode]))))

(defn explore [state]
  (fn [plan]
    (let [[final-plan _] (propagate plan state)]
      (if (empty? final-plan)
        (concat (map #(conj plan %) (keys empty-inventory))
                (for [x (range (dec (count plan)))
                      y (range x (count plan))
                      :let [left (nth plan x)
                            right (nth plan y)]]
                  (-> plan
                      (assoc x right)
                      (assoc y left)))
                (for [index (range (count plan))]
                  (into (subvec plan 0 index) (subvec plan (inc index)))))
        []))))

(defn day-19a [input]
  (apply + (for [blueprint (parse-blueprints input)
                 :let [state (initial-state blueprint)]]
             (let [result (beam-search [] ; TODO: invesitgate setting an initial state
                                       (explore state)
                                       (evaluate state)
                                       1000
                                       6000)
                   best (apply max-key (score state) (map second result))]
               (* (blueprint :id) ((score state) best))))))

(defn day-19b [input]
  (apply * (for [blueprint (take 3 (parse-blueprints input))
                 :let [state (initial-state blueprint)]]
             (let [result (beam-search [] ; TODO: invesitgate setting an initial state
                                       (explore state)
                                       (evaluate state)
                                       1000
                                       12000)
                   best (apply max-key (score state) (map second result))]
               ((score state) best)))))
