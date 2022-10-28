(ns aoc-clj.2019.01.solution
  (:require [clojure.string :as s]))

(defn floor [n] (Math/floor n))
(defn parse-int [n] (Integer/parseInt n))

(def input (map parse-int (s/split-lines (slurp "src/aoc_clj/2019/01/input.txt"))))

(defn fuel-need [mass]
  (-> mass
      (/ 3)
      floor
      (- 2)
      int))

(defn recursive-fuel-need
  ([mass] (recursive-fuel-need mass 0))
  ([mass acc]
   (let [next-mass (fuel-need mass)]
     (if (pos? next-mass)
       (recur next-mass (+ acc next-mass))
       acc))))

(defn part-1 [input]
  (->> input
       (map fuel-need)
       (apply +)))

(defn part-2 [input]
  (->> input
       (map recursive-fuel-need)
       (apply +)))

(comment
  (= 3233481 (part-1 input))
  (= 4847351 (part-2 input)))
