(ns aoc-clj.2021.07.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (slurp "src/aoc_clj/2021/07/input.txt"))
(def test-input "16,1,2,0,4,2,7,1,2,14")

(defn parse-positions [input]
  (map (comp parse-int s/trim) (s/split input #",")))

(defn distance [pos dest]
  (Math/abs (- pos dest)))

(def memo-distance (memoize distance))

(defn distance-2 [pos dest]
  (let [raw-distance (distance pos dest)]
    (reduce + (take raw-distance (map inc (range))))))

(def memo-distance-2 (memoize distance-2))

(defn spending [positions alignment]
  (reduce + (map (fn [pos] (memo-distance pos alignment)) positions)))

(defn spending-2 [positions alignment]
  (reduce + (map (fn [pos] (memo-distance-2 pos alignment)) positions)))

(defn part-1 [input]
  (let [positions (parse-positions input)
        min-pos (apply min positions)
        max-pos (apply max positions)
        alignments (range min-pos (inc max-pos))
        spendings (map (fn [alignment] (spending positions alignment)) alignments)]
    (apply min spendings)))

(defn part-2 [input]
  (let [positions (parse-positions input)
        min-pos (apply min positions)
        max-pos (apply max positions)
        alignments (range min-pos (inc max-pos))
        spendings (map (fn [alignment] (spending-2 positions alignment)) alignments)]
    (apply min spendings)))

(comment
  (= 329389 (part-1 input))
  (= 86397080 (part-2 input)))
