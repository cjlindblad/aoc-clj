(ns aoc-clj.2015.03.solution
  (:require [clojure.string :as s]))

(def input (s/split (s/trim (slurp "src/aoc_clj/2015/03/input.txt")) #""))

input

(def directions
  {"^" [0 1]
   ">" [1 0]
   "v" [0 -1]
   "<" [-1 0]})

(defn next-location [[prev-x prev-y] direction]
  (let [[x-delta y-delta] (directions direction)]
    [(+ prev-x x-delta) (+ prev-y y-delta)]))

(defn part-1 [input]
  (->> input
       (reductions next-location [0 0])
       (apply hash-set)
       count))

(defn part-2 [input]
  (let [santa-instructions (take-nth 2 input)
        robo-instructions (take-nth 2 (rest input))
        santa-locations (reductions next-location [0 0] santa-instructions)
        robo-locations (reductions next-location [0 0] robo-instructions)]
    (count (apply hash-set (concat santa-locations robo-locations)))))

(comment
  (= 2592 (part-1 input))
  (= 2360 (part-2 input)))
