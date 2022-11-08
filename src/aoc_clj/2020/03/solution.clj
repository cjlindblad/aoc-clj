(ns aoc-clj.2020.03.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2020/03/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2020/03/test-input.txt")))

(defn calculate-slope
  ([area x-inc y-inc] (calculate-slope area [] 0 0 x-inc y-inc))
  ([area slope x y x-inc y-inc]
   (if (>= y (count area)) slope
       (let [line (get area y)
             line-index (mod x (count line))
             next-slope (conj slope (get line line-index))]
         (recur area next-slope (+ x x-inc) (+ y y-inc) x-inc y-inc)))))

(defn filter-trees [xs]
  (filter #(= % \#) xs))

(defn part-1 [input]
  (->> (calculate-slope input 3 1)
       filter-trees
       count))

(defn part-2 [input]
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
       (map (fn [[x-inc y-inc]] (calculate-slope input x-inc y-inc)))
       (map filter-trees)
       (map count)
       (reduce *)))

(comment
  (= 299 (part-1 input))
  (= 3621285278 (part-2 input)))
