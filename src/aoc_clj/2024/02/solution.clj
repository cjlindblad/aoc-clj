(ns aoc-clj.2024.02.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/02/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/02/test-input.txt"))

(defn valid-diffs? [report]
  (->> (partition 2 1 report)
       (map (partial apply -))
       (map #(Math/abs %))
       (remove #{1 2 3})
       count
       zero?))

(defn safe? [report]
  (and (or (apply < report) (apply > report))
       (valid-diffs? report)))

(defn problem-dampen [report]
  (for [i (range (count report))]
    (concat (take i report)
            (take (- (count report) i) (drop (inc i) report)))))

(defn safe-2? [report]
  (let [variants (problem-dampen report)]
    (some safe? variants)))

(defn solver [f input]
  (->> (str/split input #"\n")
       (mapv #(read-string (str "[" % "]")))
       (filter f)
       count))

(def part-1 (partial solver safe?))
(def part-2 (partial solver safe-2?))

(comment
  (= 598 (part-1 input))
  (= 634 (part-2 input)))
