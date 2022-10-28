(ns aoc-clj.utils)

(defn first-duplicate
  ([xs] (first-duplicate xs #{}))
  ([xs seen]
   (if (empty? xs) nil
       (let [x (first xs)]
         (if (seen x) x
             (recur (rest xs) (conj seen x)))))))
