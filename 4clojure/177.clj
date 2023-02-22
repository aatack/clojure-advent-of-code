(defn __ [string]
  (let [bracket-type {\( [:parenthesis true]
                      \) [:parenthesis false]
                      \[ [:bracket true]
                      \] [:bracket false]
                      \{ [:brace true]
                      \} [:brace false]}]
    (boolean (reduce (fn [stack [kind open]]
                       (cond
                         open (cons kind stack)
                         (empty? stack) (reduced false)
                         (= kind (first stack)) (rest stack)
                         :else (reduced false)))
                     ()
                     (map bracket-type (filter bracket-type string))))))
