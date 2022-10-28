(ns aoc-clj.2020.01.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (map parse-int (s/split-lines (slurp "src/aoc_clj/2020/01/input.txt"))))

(defn part-1 [input]
  (first
   (for [x input
         y input
         :when (= 2020 (+ x y))]
     (* x y))))

(defn part-2 [input]
  (first
   (for [x input
         y input
         z input
         :when (= 2020 (+ x y z))]
     (* x y z))))

(comment
  (= 996996 (part-1 input))
  (= 9210402 (part-2 input)))
