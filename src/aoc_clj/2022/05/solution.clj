(ns aoc-clj.2022.05.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/05/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/05/test-input.txt")))

(defn parse-instruction [line]
  (let [[amount from to] (re-seq #"\d+" line)]
    {:amount (Integer/parseInt amount)
     :from (Integer/parseInt from)
     :to (Integer/parseInt to)}))

(defn parse-stack [stack-lines]
  (let [stack (->> (apply map vector stack-lines)
                   (map reverse)
                   (filter #(Character/isDigit (first %)))
                   (map rest)
                   (map (partial filter #(not= \space %))))]
    (zipmap (map inc (range)) stack)))

(defn parse-input [input]
  (let [[raw-stacks _ instructions] (partition-by #(pos? (count %)) input)]
    {:stacks (parse-stack raw-stacks)
     :instructions (map parse-instruction instructions)}))

(defn next-stacks [variant stacks {:keys [amount from to]}]
  (let [to-move (take-last amount (stacks from))
        removed-stack {from (drop-last amount (stacks from))}
        added-stack {to (concat (stacks to) (variant to-move))}]
    (merge stacks removed-stack added-stack)))

(defn solver [variant input]
  (let [{:keys [stacks instructions]} (parse-input input)
        final-state (reduce (partial next-stacks variant) stacks instructions)
        ordered-stacks (map final-state (take (count final-state) (map inc (range))))]
    (apply str (map last ordered-stacks))))

(def part-1 (partial solver reverse))
(def part-2 (partial solver identity))

(comment
  (= "QNNTGTPFN" (part-1 input))
  (= "GGNPJBTTR" (part-2 input)))
