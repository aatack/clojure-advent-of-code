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

(defn cells-have-gap? [pipes left right]
  (-> pipes (get left) :connects (get right) not))

(defn connecting-corners [pipes corners]
  (fn [[x y]]
    (filter corners
            [(when (cells-have-gap? pipes [(dec x) (dec y)] [(dec x) y]) [(dec x) y])
             (when (cells-have-gap? pipes [x (dec y)] [x y]) [(inc x) y])
             (when (cells-have-gap? pipes [(dec x) (dec y)] [x (dec y)]) [x (dec y)])
             (when (cells-have-gap? pipes [(dec x) y] [x y]) [x (inc y)])])))

(defn propagate [node accept? children]
  (loop [waiting #{node}
         accepted #{}
         rejected #{}
         times 0]
    (if (empty? waiting)
      accepted
      (let [node' (first waiting)
            accept (accept? node')]
        (recur (apply conj (rest waiting) (->> (if accept (children node') [])
                                               (remove accepted)
                                               (remove rejected)))
               (if accept (doall (conj accepted node')) accepted)
               (if accept rejected (doall (conj rejected node')))
               (inc times))))))

(defn cache-connections [input]
  (let [pipes (parse-pipes input)
        animal (animal-coordinates pipes)
        distances (populate-distances (assoc-in pipes [animal :distance] 0)
                                      (connecting-coordinates pipes
                                                              [animal (pipes animal)]))]
    (->> distances
         (map
          (fn [[coordinate pipe]]
            [coordinate
             (assoc pipe
                    :connects
                    (into #{} (connecting-coordinates
                               distances
                               [coordinate (distances coordinate)])))]))
         (into {}))))

(defn has-corner? [corners [x y]]
  (some corners [[x y] [(inc x) y] [x (inc y)] [(inc x) (inc y)]]))

(defn day-10b [input]
  (let [pipes (cache-connections input)
        width (->> pipes keys (map first) (apply max) inc)
        height (->> pipes keys (map second) (apply max) inc)
        corners (into #{} (for [x (range (inc width))
                                y (range (inc height))]
                            [x y]))
        outside-corners (propagate [0 0]
                                   (constantly true)
                                   (connecting-corners pipes corners))]
    (->> pipes
           (filter (fn [[coordinate {:keys [pipe]}]]
                     (and (= pipe \.) (not (has-corner? outside-corners coordinate)))))
           count)))
