(ns aoc-clj.2022.01.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/01/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/01/test-input.txt")))

(defn parse-int [n] (Integer/parseInt n))

(defn mapf [f' f coll] (map (fn [el] (f' f el)) coll))
(def mapmap (partial mapf map))
(def mapreduce (partial mapf reduce))

(defn calories-by-elf [input]
  (let [string-groups (->> (partition-by empty? input)
                           (filter #(not= [""] %)))
        number-groups (mapmap parse-int string-groups)
        sum-groups (mapreduce + number-groups)]
    sum-groups))

(defn part-1 [input]
  (apply max (calories-by-elf input)))

(defn part-2 [input]
  (->> (sort (calories-by-elf input))
       (take-last 3)
       (reduce +)))

(comment
  (= 72017 (part-1 input))
  (= 212520 (part-2 input)))

