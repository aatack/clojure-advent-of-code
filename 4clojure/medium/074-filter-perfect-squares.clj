(require 'clojure.string)

(def __
  (fn [string]
    (as-> string $
      (clojure.string/split $ #",")
      (map #(Integer/parseInt %) $)
      (filter #(= (mod (Math/sqrt %) 1.0) 0.0) $)
      (clojure.string/join "," $))))

(= (__ "4,5,6,7,8,9") "4,9")
(= (__ "15,16,25,36,37") "16,25,36")
