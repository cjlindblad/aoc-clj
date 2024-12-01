(ns aoc-clj.2024.01.solution
  (:require [clojure.string :as str]))

(def input (str/split (slurp "src/aoc_clj/2024/01/input.txt") #"\n"))

(defn parse-input [input]
  (->> (map #(str/split % #"\s+") input)
       (map (partial map parse-long))
       (reduce (fn [[xs ys] [x y]] [(conj xs x) (conj ys y)]) [[] []])))

(defn part-1 [input]
  (->> (parse-input input)
       (map sort)
       (apply map (fn [x y] (Math/abs (- x y))))
       (reduce +)))

(defn similarity-score [xs ys]
  (map #(-> (filter #{%} ys) count (* %)) xs))

(defn part-2 [input]
  (->> (parse-input input)
       (apply similarity-score)
       (reduce +)))

(comment
  (= 2000468 (part-1 input))
  (= 18567089 (part-2 input)))

