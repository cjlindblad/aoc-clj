(ns aoc-clj.2022.01.solution
  (:require [clojure.string :as string]))

(def input (slurp "src/aoc_clj/2022/01/input.txt"))
(def test-input (slurp "src/aoc_clj/2022/01/test-input.txt"))

(defn calories-by-elf [input]
  (->> (string/split input #"\n\n")
       (map #(string/split % #"\n"))
       (map (partial map #(Integer/parseInt %)))
       (map (partial reduce +))))

(defn part-1 [input] (apply max (calories-by-elf input)))

(defn part-2 [input]
  (->> (sort (calories-by-elf input))
       (take-last 3)
       (reduce +)))

(comment
  (= 72017 (part-1 input))
  (= 212520 (part-2 input)))

