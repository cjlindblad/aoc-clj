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

(defn vec-add [[a1 a2] [b1 b2]]
  [(+ a1 b1) (+ a2 b2)])

(defn vec-mult [[a1 a2] n]
  [(* a1 n) (* a2 n)])

(defn valid-sequences [button-a button-b goal]
  (for [a-count (range 201)
        b-count (range 201)
        :when (and
               (<= (+ a-count b-count) 200)
               (= goal (vec-add (vec-mult button-a a-count) (vec-mult  button-b b-count))))]
    [a-count b-count]))

(defn sequence-cost [sequence]
  (map (fn [[a b]] (+ (* 3 a) b)) sequence))

(defn cheapest-sequence [sequences]
  (->> sequences
       sort
       first))

(defn part-1 [input]
  (let [prizes (parse-input input)
        sequences (map (fn [[a b goal]] (valid-sequences a b goal)) prizes)
        costs (map sequence-cost sequences)
        cheapest (filter identity (map cheapest-sequence costs))]
    (reduce + cheapest)))

(comment
  (= 29187 (part-1 input)))
