(ns aoc-clj.2024.13.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/13/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/13/test-input.txt"))
test-input

(defn parse-long [n] (Long/parseLong n))

(defn parse-prize [prize-input]
  (let [lines (str/split prize-input #"\n")]
    (->> (map #(re-seq #"(\d+)\D+(\d+)" %) lines)
         (mapcat (partial map (partial drop 1)))
         (map (partial map parse-long)))))

(defn parse-input [input]
  (let [prizes (str/split input #"\n\n")]
    (map parse-prize prizes)))

(defn get-cost [[[ax ay] [bx by] [px py]]]
  (let [b (/ (- (* py ax) (* px ay)) (- (* by ax) (* bx ay)))
        a (/ (- px (* b bx)) ax)]
    [a b]))

(defn solver [prizes]
  (let [presses (map get-cost prizes)
        valid-presses (filter (fn [[a b]] (and (int? a) (int? b))) presses)
        costs (map (fn [[a b]] (+ (* 3 a) b)) valid-presses)]
    (reduce + costs)))

(defn part-1 [input]
  (let [prizes (parse-input input)]
    (solver prizes)))

(defn part-2 [input]
  (let [prizes (parse-input input)
        huge-prizes (map (fn [[a b [px py]]] [a b [(+ px 10000000000000) (+ py 10000000000000)]]) prizes)]
    (solver huge-prizes)))

(comment
  (= 29187 (part-1 input))
  (= 99968222587852 (part-2 input)))
