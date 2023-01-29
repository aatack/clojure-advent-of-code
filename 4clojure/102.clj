(require '[clojure.string :as str])

(defn __ [identifier]
  (let [segments (str/split identifier #"-")
        capitalised (cons (first segments) (map str/capitalize (rest segments)))]
    (apply str capitalised)))

(= (__ "something") "something")
(= (__ "multi-word-key") "multiWordKey")
(= (__ "leaveMeAlone") "leaveMeAlone")
