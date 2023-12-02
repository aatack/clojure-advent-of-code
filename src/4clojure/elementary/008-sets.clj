(def __ #{:a :b :c :d})

(= __ (set '(:a :a :b :c :c :c :c :d :d)))
(= __ #_{:clj-kondo/ignore [:unresolved-namespace]}
   (clojure.set/union #{:a :b :c} #{:b :c :d}))
