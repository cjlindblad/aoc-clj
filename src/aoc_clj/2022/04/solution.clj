(ns aoc-clj.2022.04.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/04/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/04/test-input.txt")))

(defn parse-section-sets [line]
  (let [[from-1 to-1 from-2 to-2] (string/split line #"\D")]
    [(set (range (Integer/parseInt from-1) (inc (Integer/parseInt to-1))))
     (set (range (Integer/parseInt from-2) (inc (Integer/parseInt to-2))))]))

(defn part-1 [input]
  (->> (map parse-section-sets input)
       (filter (fn [[a b]] (or (set/subset? a b) (set/subset? b a))))
       count))

(defn part-2 [input]
  (->> (map parse-section-sets input)
       (filter (fn [[a b]] (not-empty (set/intersection a b))))
       count))

(comment
  (= 599 (part-1 input))
  (= 928 (part-2 input)))
