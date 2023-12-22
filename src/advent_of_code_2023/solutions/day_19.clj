(ns advent-of-code-2023.solutions.day-19
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [advent-of-code-2023.utils :refer [split-string]]
            [clojure.string :refer [includes? split-lines]]))

(defn parse-instruction [instruction]
  (let [guarded-instruction (if (includes? instruction ":")
                              instruction
                              (str "x>0:" instruction))
        [condition result] (split-string ":" guarded-instruction)]
    {:greater (= (second condition) \>)
     :variable (-> condition first str read-string)
     :value (-> condition (subs 2) read-string)
     :result result}))

(defn parse-workflow [workflow]
  (let [[name instructions] (split-string "\\{" (subs workflow
                                                      0
                                                      (dec (count workflow))))]
    [name (map parse-instruction (split-string "," instructions))]))

(defn parse-part [part]
  (read-string (clojure.string/replace part #"=" " ")))

(defn accepted? [workflows part]
  (loop [steps (workflows "in")]
    (let [{:keys [greater variable value result]} (first steps)
          workflow (when ((if greater > <) (part variable) value) result)]
      (case workflow
        nil (recur (rest steps))
        "A" true
        "R" false
        (recur (workflows workflow))))))

(defn score [part]
  (->> part vals (apply +)))

(defn day-19a [input]
  (let [[workflows-input parts-input] (parse-chunks input)
        parts (map parse-part parts-input)
        workflows (into {} (map parse-workflow workflows-input))]
    (->> parts
         (filter #(accepted? workflows %))
         (map score)
         (apply +))))

#_(defn accepted-space [instructions workflow]
    (if (empty? instructions)
      {'x [1 4000]
       'm [1 4000]
       'a [1 4000]
       's [1 4000]}
      (let [{:keys [greater variable value result]} (first instructions)
            spaces (accepted-space ())])))

(def flatten-workflow
  (memoize
   (fn [workflows workflow]
     (if (empty? workflow)
       :reject
       (let [{:keys [greater variable value result]} (first workflow)]
         {:variable variable
          :range (if greater [(inc value) 4000] [1 (dec value)])
          :inside (case result
                    "A" :accept
                    "B" :reject
                    (flatten-workflow workflows (workflows result)))
          :outside (flatten-workflow workflows (rest workflow))})))))

(defn day-19b [input]
  (let [workflows (->> input parse-chunks first (map parse-workflow) (into {}))]
    (flatten-workflow workflows (workflows "hdj"))))
