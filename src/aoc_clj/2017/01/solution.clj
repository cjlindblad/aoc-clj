(ns aoc-clj.2017.01.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (map parse-int (s/split (s/trim (slurp "src/aoc_clj/2017/01/input.txt")) #"")))

(defn wrap-around [ns]
  (->> (cycle ns)
       (take (inc (count ns)))))

(defn part-1 [input]
  (->> input
       wrap-around
       (partition 2 1)
       (filter #(apply = %))
       (map first)
       (apply +)))

(defn shift [xs]
  (->> (cycle xs)
       (drop (/ (count xs) 2))
       (take (count xs))))

(defn part-2 [input]
  (let [shifted-input (shift input)
        zipped (map vector input shifted-input)
        matches (filter #(reduce = %) zipped)
        digits (map first matches)]
    (apply + digits)))

(comment
  (= 1393 (part-1 input))
  (= 1292 (part-2 input)))
