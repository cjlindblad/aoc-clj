(ns aoc-clj.2022.08.solution
  (:require [clojure.string :as string]
            [aoc-clj.utils :as utils]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/08/test-input.txt")))

(defn parse-map [input]
  (map (partial map #(Integer/parseInt (str %))) input))

(defn visible? [trees n]
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
        south-line (drop (inc vertical-index) vertical-line)]
    (or (every? #(< % tree) west-line)
        (every? #(< % tree) east-line)
        (every? #(< % tree) north-line)
        (every? #(< % tree) south-line))))

(defn scenic-score [trees n]
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
        south-line (drop (inc vertical-index) vertical-line)]
    (*
     (count (utils/take-upto #(>= % tree) east-line))
     (count (utils/take-upto #(>= % tree) south-line))
     (count (utils/take-upto #(>= % tree) (reverse north-line)))
     (count (utils/take-upto #(>= % tree) (reverse west-line))))))

(defn part-1 [input]
  (let [tree-map (parse-map input)
        visible (filter (fn [i] (visible? tree-map i)) (range 0 (count (flatten tree-map))))]
    (count visible)))

(defn part-2 [input]
  (let [tree-map (parse-map input)
        scenic-scores (map (fn [i] (scenic-score tree-map i)) (range 0 (count (flatten tree-map))))]
    (apply max scenic-scores)))

(comment
  (= 1693 (part-1 input))
  (= 422059 (part-2 input)))

