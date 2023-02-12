(def __ (fn [] (let [program '(fn [] (let [program ':r]
                                       (clojure.string/replace-first
                                        (str program) #":r" (str program))))]
                 (clojure.string/replace-first (str program) #":r" (str program)))))

(= (str '(fn [] (let [program '(fn [] (let [program ':r]
                                        (clojure.string/replace-first
                                         (str program) #":r" (str program))))]
                  (clojure.string/replace-first (str program) #":r" (str program)))))
   ((fn [] (let [program '(fn [] (let [program ':r]
                                   (clojure.string/replace-first
                                    (str program) #":r" (str program))))]
             (clojure.string/replace-first (str program) #":r" (str program))))))
