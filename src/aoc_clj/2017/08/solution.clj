(ns aoc-clj.2017.08.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2017/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2017/08/test-input.txt")))

(defn parse-instruction [line]
  (let [[reg op op-val _ pred-reg pred-op pred-val] (string/split line #" ")]
    {:reg reg
     :op (if (= op "inc") + -)
     :op-val (Integer/parseInt op-val)
     :pred-reg pred-reg
     :pred-op (case pred-op
                ">" >
                "<" <
                ">=" >=
                "<=" <=
                "==" =
                "!=" not=)
     :pred-val (Integer/parseInt pred-val)}))

(defn apply-instruction [registers {:keys [reg op op-val pred-reg pred-op pred-val]}]
  (if (pred-op (registers pred-reg 0) pred-val)
    (assoc registers reg (op (registers reg 0) op-val))
    registers))

(defn part-1 [input]
  (let [instructions (map parse-instruction input)
        result (reduce apply-instruction {} instructions)]
    (apply max (vals result))))

(defn part-2 [input]
  (let [instructions (map parse-instruction input)
        results (reductions apply-instruction {} instructions)
        result-vals (mapcat vals results)]
    (apply max result-vals)))

(comment
  (= 5849 (part-1 input))
  (= 6702 (part-2 input)))
