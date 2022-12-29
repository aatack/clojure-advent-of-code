(require '[clojure.math.combinatorics :as combo])

(defn chainable-by-deletion [left right]
  (and (= (count left) (inc (count right)))
       (some (fn [index]
               (= (concat (take index left) (drop (inc index) left))
                  (apply list right)))
             (range (inc (count right))))))

(defn chainable-by-replacement [left right]
  (and (= (count left) (count right))
       (some (fn [index]
               (chainable-by-deletion
                left
                (apply str (concat (take index right)
                                   (drop (inc index) right)))))
             (range (inc (count right))))))

(defn chainable [left right]
  (or (chainable-by-deletion left right)
      (chainable-by-deletion right left)
      (chainable-by-replacement left right)))

(defn pairs [sequence]
  (map vector (butlast sequence) (rest sequence)))

(defn chain-chainable [chain]
  (every? #(chainable (first %) (second %))
          (pairs chain)))

(defn permutations [sequence]
  (if (< (count sequence) 2)
    [sequence]
    ;; TODO: find a better way of excluding duplicates
    (set (apply
          concat
          (map (fn [drop-index]
                 (let [head (take drop-index sequence)
                       tail (drop (inc drop-index) sequence)
                       pivot (nth sequence drop-index)]
                   (for [permutation (permutations (concat head tail))
                         insert-index (range (inc (count sequence)))]
                     (concat (take insert-index permutation)
                             [pivot]
                             (drop insert-index permutation)))))
               (range (count sequence)))))))

(defn has-valid-word-chain [words]
  (boolean (some chain-chainable
                 (permutations (apply vector words)))))

(def __ has-valid-word-chain)

(filter chain-chainable
        (permutations (apply vector #{"to" "top" "stop" "tops" "toss"})))

(chainable-by-replacement "stop" "tops")

(= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
(= false (__ #{"cot" "hot" "bat" "fat"}))
(= false (__ #{"to" "top" "stop" "tops" "toss"}))
(= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (__ #{"share" "hares" "shares" "hare" "are"}))
(= false (__ #{"share" "hares" "hare" "are"}))
