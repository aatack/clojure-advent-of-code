(ns advent-of-code-2023.solutions.day-10
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-grid]]
            [clojure.string :refer [split-lines]]))

(defn parse-pipes [input]
  (->> input
       parse-grid
       (map (fn [[coordinate pipe]] [coordinate {:pipe pipe}]))
       (into {})))

(defn animal-coordinates [pipes]
  (->> pipes
       (filter #(= (-> % val :pipe) \S))
       first
       key))

(defn connecting-coordinates [pipes [[x y] {:keys [pipe]}]]
  (case pipe
    nil []
    \| [[x (dec y)] [x (inc y)]]
    \- [[(dec x) y] [(inc x) y]]
    \L [[x (dec y)] [(inc x) y]]
    \J [[(dec x) y] [x (dec y)]]
    \7 [[(dec x) y] [x (inc y)]]
    \F [[(inc x) y] [x (inc y)]]
    \S (into []
             (filter #((set (connecting-coordinates pipes [% (pipes %)])) [x y])
                     [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))
    []))

(defn populate-distances [initial-pipes initial-coordinates]
  (loop [pipes initial-pipes
         coordinates initial-coordinates]
    (if (empty? coordinates)
      pipes
      (let [coordinate (first coordinates)
            candidates (connecting-coordinates pipes
                                               [coordinate (pipes coordinate)])
            distance (->> candidates
                          (map #(get-in pipes [% :distance]))
                          (remove nil?)
                          (apply min)
                          inc)]
        (recur (assoc-in pipes [coordinate :distance] distance)
               (concat (rest coordinates)
                       (->> candidates
                            (filter #(not (get-in pipes [% :distance]))))))))))

(defn day-10a [input]
  (let [pipes (parse-pipes input)
        animal (animal-coordinates pipes)]
    (->> (populate-distances (assoc-in pipes [animal :distance] 0)
                             (connecting-coordinates pipes [animal (pipes animal)]))
         vals
         (map :distance)
         (remove nil?)
         (apply max))))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn outside? [pipes coordinate]
  (some nil? (map pipes (neighbours coordinate))))

(defn propagate [node accept? children]
  (loop [waiting #{node}
         accepted #{}
         rejected #{}
         times 0]
    (if (or (empty? waiting) (> times 100))
      accepted
      (let [node' (first waiting)
            accept (accept? node')]
        (recur (apply conj (rest waiting) (->> (if accept (children node') [])
                                               (remove accepted)
                                               (remove rejected)))
               (if accept (conj accepted node') accepted)
               (if accept rejected (conj rejected node'))
               (inc times))))))

(defn partition-regions [pipes]
  (loop [regions []
         coordinates (keys pipes)]
    (if (empty? coordinates)
      (remove empty? regions)
      (let [coordinate (first coordinates)
            region (propagate coordinate
                              #(and (not (get-in pipes [% :distance])) (pipes %))
                              #(filter pipes (neighbours %)))]
        (recur (conj regions region)
               (->> coordinates rest (remove region)))))))

(defn day-10b [input]
  (let [pipes (parse-pipes input)
        animal (animal-coordinates pipes)
        distances (populate-distances (assoc-in pipes [animal :distance] 0)
                                      (connecting-coordinates pipes
                                                              [animal (pipes animal)]))]
    (->> distances
         partition-regions)))
