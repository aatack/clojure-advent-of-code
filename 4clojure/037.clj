(def __ "ABC")

(= __ (apply str (re-seq #"[A-Z]+" "bA1B3Ce ")))
