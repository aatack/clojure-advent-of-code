(require '[clojure.string :as str])

(defn __ [word board]
  (letfn [(spaces-from-line [line]
            (->> line
                 (partition-by #(= % "#"))
                 (filter #(and (> (count %) 1)
                               (not ((set %) "#"))))))]
    
    (let [rows (map #(str/split % #" ") board)
          columns (apply map vector rows)
          lines (concat rows columns)
          spaces (apply concat (map spaces-from-line lines))]
      
      spaces)))

(__ "clojure" ["_ _ _ # j o y"
               "_ _ o _ _ _ _"
               "_ _ f _ # _ _"])
