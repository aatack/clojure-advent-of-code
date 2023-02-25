(def __ (fn [string]
          (apply str (re-seq #"[A-Z]" string))))

(= (__ "HeLlO, WoRlD!") "HLOWRD")
(empty? (__ "nothing"))
(= (__ "$#A(*&987Zf") "AZ")
