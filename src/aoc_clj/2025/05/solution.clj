(ns aoc-clj.2025.05.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2025/05/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/05/test-input.txt"))

(defn parse-range [range]
  (let [[_ from to] (first (re-seq #"(\d+)-(\d+)" range))]
    {:from (Long/parseLong from) :to (Long/parseLong to)}))

(defn parse-input [input]
  (let [[ranges ids] (->> (str/split input #"\n\n")
                          (map #(str/split % #"\n")))
        ranges (mapv parse-range ranges)
        ids (mapv #(Long/parseLong %) ids)]
    {:ranges ranges :ids ids}))

(defn solver-1 [input]
  (let [{:keys [ranges ids]} (parse-input input)
        fresh-ids (filter (fn [id]
                            (reduce (fn [valid {:keys [from to]}]
                                      (or valid (and (>= id from) (<= id to)))) false ranges)) ids)]
    (count fresh-ids)))

(comment
  (= 563 (solver-1 input)))
