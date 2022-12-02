(ns aoc-clj.2022.02.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/02/input.txt")))
(def test-input ["A Y" "B X" "C Z"])

(def shape-scores {"X" 1 "Y" 2 "Z" 3})
(def draw-rounds-set  #{["A" "X"] ["B" "Y"] ["C" "Z"]})
(def win-rounds-set #{["C" "X"] ["A" "Y"] ["B" "Z"]})

(defn round-score [[_ elf :as round]]
  (let [shape-score (shape-scores elf)
        outcome-score (cond (draw-rounds-set round) 3
                            (win-rounds-set round) 6
                            :else 0)]
    (+ shape-score outcome-score)))

(def needed-shape
  {"A" {"X" "Z", "Y" "X", "Z" "Y"}
   "B" {"X" "X", "Y" "Y", "Z" "Z"}
   "C" {"X" "Y", "Y" "Z", "Z" "X"}})

(defn round-score-2 [[_ ending :as round]]
  (let [shape-score (shape-scores (get-in needed-shape round))
        outcome-score ({"X" 0 "Y" 3 "Z" 6} ending)]
    (+ shape-score outcome-score)))

(defn solver [score-function input]
  (let [rounds (map #(string/split % #" ") input)
        score (map score-function rounds)]
    (reduce + score)))

(def part-1 (partial solver round-score))
(def part-2 (partial solver round-score-2))

(comment
  (= 15572 (part-1 input))
  (= 16098 (part-2 input)))
