(def __ '(x y))

;; NOTE: the solution doesn't really evaluate as-is; it would need
;;       escaping to be done to make it work OOTB

(= 3
   (let [[__] [+ (range 3)]] (apply __))
   (let [[[__] b] [[+ 1] 2]] (__ b))
   (let [[__] [inc 2]] (__)))