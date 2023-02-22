(defn __ [string]
  (let [bracket-type {\( [:parenthesis true]
                      \) [:parenthesis false]
                      \[ [:bracket true]
                      \] [:bracket false]
                      \{ [:brace true]
                      \} [:brace false]}]
    (empty? (reduce (fn [stack [kind open]]
                      (cond
                        open (cons kind stack)
                        (empty? stack) (reduced [nil])
                        (= kind (first stack)) (rest stack)
                        :else (reduced [nil])))
                    ()
                    (map bracket-type (filter bracket-type string))))))

(__ "This string has no brackets.")
(__ "class Test {
         public static void main(String[] args) {
             System.out.println(\"Hello world.\");
         }
     }")
(not (__ "(start, end]"))
(not (__ "())"))
(not (__ "[ { ] } "))
(__ "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))")
(not (__ "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))"))
(not (__ "["))
