(defn __ [key lookup]
  (boolean (and (not (lookup key))
                (some #(= key %) (keys lookup)))))

(true? (__ :a {:a nil :b 2}))
(false? (__ :b {:a nil :b 2}))
(false? (__ :c {:a nil :b 2}))
