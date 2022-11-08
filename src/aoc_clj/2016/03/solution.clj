(ns aoc-clj.2016.03.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input
  (as-> (slurp "src/aoc_clj/2016/03/input.txt") $
    (s/trim $)
    (s/split $ #"\s+")
    (mapv parse-int $)))

(defn possible-triangle? [[a b c]]
  (and
   (> (+ a b) c)
   (> (+ b c) a)
   (> (+ a c) b)))

(defn part-1 [input]
  (->> input
       (partition 3)
       (filter possible-triangle?)
       count))

(defn part-2 [input]
  (let [row-1 (take-nth 3 input)
        row-2 (take-nth 3 (drop 1 input))
        row-3 (take-nth 3 (drop 2 input))]
    (part-1 (concat row-1 row-2 row-3))))

(comment
  (= 1032 (part-1 input))
  (= 1838 (part-2 input)))
