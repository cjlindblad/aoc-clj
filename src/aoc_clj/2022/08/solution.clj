(ns aoc-clj.2022.08.solution
  (:require [clojure.string :as string]
            [aoc-clj.utils :as utils]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/08/test-input.txt")))

(defn parse-map [input]
  (map (partial map #(Integer/parseInt (str %))) input))

(defn find-good-spots [trees n]
  (let [width (count (first trees))
        horizontal-line (nth trees (quot n width))
        horizontal-index (mod n width)
        tree (nth horizontal-line horizontal-index)
        rotated (apply map vector trees)
        vertical-line (nth rotated (mod n width))
        vertical-index (quot n width)
        west-line (take horizontal-index horizontal-line)
        east-line (drop (inc horizontal-index) horizontal-line)
        north-line (take vertical-index vertical-line)
        south-line (drop (inc vertical-index) vertical-line)
        visible-lines [west-line east-line north-line south-line]
        scenic-lines [east-line south-line (reverse north-line) (reverse west-line)]]
    {:visible (->> (map (partial every? #(< % tree)) visible-lines)
                   (some identity))
     :scenic-score (->> (map (comp count (partial utils/take-upto #(>= % tree))) scenic-lines)
                        (reduce *))}))

(defn solver [input]
  (let [tree-map (parse-map input)
        result (map (partial find-good-spots tree-map) (range 0 (count (flatten tree-map))))]
    {:visible-count (count (filter :visible result))
     :scenic-score (apply max (map :scenic-score result))}))

(defn part-1 [input] (:visible-count (solver input)))
(defn part-2 [input] (:scenic-score (solver input)))

(comment
  (= 1693 (part-1 input))
  (= 422059 (part-2 input)))
