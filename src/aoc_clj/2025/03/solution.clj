(ns aoc-clj.2025.03.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2025/03/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/03/test-input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map (partial mapv #(Character/digit % 10)))
       ))

(defn largest-joltage [bank]
  (let [first-largest (first (reverse (sort (drop-last 1 bank))))
        first-largest-index (.indexOf bank first-largest)
        second-largest (first (reverse (sort (drop (inc first-largest-index) bank))))]
    (Long/parseLong (str first-largest second-largest))))

(defn solver-1 [input]
  (->> (parse-input input)
       (map largest-joltage)
       (reduce +)
       )
  )

(comment
  (= 17229 (solver-1 input)))

