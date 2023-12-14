(ns advent-of-code-2023.solutions.day-13
  #_{:clj-kondo/ignore [:unused-referred-var :unused-namespace]}
  (:require [advent-of-code-2023.parsing :refer [parse-chunks]]
            [advent-of-code-2023.utils :refer [transpose-lines]]
            [clojure.string :refer [split-lines]]))

(defn reflection-row [lines]
  (loop [above [(first lines)]
         below (rest lines)]
    (cond
      (empty? below) nil
      (every? identity (map = above below)) (count above)
      :else (recur (cons (first below) above) (rest below)))))

(defn reflection-score [lines]
  (let [row (reflection-row lines)]
    (if row
      (* 100 row)
      (let [column (reflection-row (transpose-lines lines))]
        (or column 0)))))

(defn day-13a [input]
  (->> input
       parse-chunks
       (map reflection-score)
       (apply +)))

(defn update-string [string index function]
  (apply str (update (into [] (map identity string)) index function)))

(defn changes [lines]
  (for [row (range (count lines))
        column (range (count (first lines)))]
    (update lines
            row
            #(update-string % column {\# \., \. \#}))))

(defn reflection-rows [lines]
  (for [rows (range 1 (count lines))
        :when (every? identity (map = (reverse (take rows lines)) (drop rows lines)))]
    rows))

(defn reflection-columns [lines]
  (->> lines transpose-lines reflection-rows))

(defn reflections [lines]
  (concat (map (fn [n] {:row n}) (reflection-rows lines))
          (map (fn [n] {:column n}) (reflection-columns lines))))

(defn changed-reflection [lines]
  (let [original (into #{} (reflections lines))]
    (let [changed-reflections (->> lines
                                   (into [])
                                   changes
                                   (mapcat reflections)
                                   (into #{})
                                   (remove original))]
      (assert (= (count changed-reflections) 1))
      (first changed-reflections))))

(defn score [{:keys [row column]}]
  (+ (* 100 (or row 0)) (or column 0)))

(defn day-13b [input]
  (->> input
       parse-chunks
       (map changed-reflection)
       (map score)
       (apply +)))
