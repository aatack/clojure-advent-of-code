(require '[clojure.string :as str])

(defn __ [word board]
  (letfn [(spaces-from-line [line]
            (->> line
                 (partition-by #(= % "#"))
                 (filter #(and (> (count %) 1)
                               (not ((set %) "#"))))))

          (word-matches-space [space]
            (and (= (count word) (count space))
                 (every? identity (map #(or (= %1 "_") (= %1 %2))
                                       space
                                       (map str word)))))]

    (let [rows (map #(str/split % #" ") board)
          columns (apply map vector rows)
          lines (concat rows columns)
          spaces (apply concat (map spaces-from-line lines))]

      (boolean (some word-matches-space spaces)))))

(__ "clojure" ["_ _ _ # j o y"
               "_ _ o _ _ _ _"
               "_ _ f _ # _ _"])
