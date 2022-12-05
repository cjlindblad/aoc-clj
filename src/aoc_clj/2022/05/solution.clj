(ns aoc-clj.2022.05.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/05/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/05/test-input.txt")))

(defn parse-instruction [line]
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

(defn parse-input [input]
  (let [[stack-lines _ instrs] (partition-by #(pos? (count %)) input)]
    [(->> (apply map vector stack-lines)
          (map reverse)
          (filter #(Character/isDigit (first %)))
          (map (partial filter #(not= \space %)))
          (zipmap (map inc (range))))
     (map parse-instruction instrs)]))

(defn next-stacks [variant stacks [amount from to]]
  (let [to-move (take-last amount (stacks from))
        removed-stack {from (drop-last amount (stacks from))}
        added-stack {to (concat (stacks to) (variant to-move))}]
    (merge stacks removed-stack added-stack)))

(defn solver [variant input]
  (let [final-state (apply reduce (partial next-stacks variant) (parse-input input))
        ordered-stacks (map final-state (take (count final-state) (map inc (range))))]
    (apply str (map last ordered-stacks))))

(def part-1 (partial solver reverse))
(def part-2 (partial solver identity))

(comment
  (= "QNNTGTPFN" (part-1 input))
  (= "GGNPJBTTR" (part-2 input)))
