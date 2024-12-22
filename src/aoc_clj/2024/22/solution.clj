(ns aoc-clj.2024.22.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/22/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/22/test-input.txt"))

(defn parse-input [input]
  (mapv read-string (str/split input #"\n")))

(defn mix [a b]
  (bit-xor a b))

(defn prune [n]
  (bit-and n 2r111111111111111111111111))

(defn step-1 [n]
  (->> (bit-shift-left n 6)
       (mix n)
       (prune)))

(defn step-2 [n]
  (->> (bit-shift-right n 5)
       (mix n)
       (prune)))

(defn step-3 [n]
  (->> (bit-shift-left n 11)
       (mix n)
       (prune)))

(defn process [n]
  (-> (step-1 n)
      (step-2)
      (step-3)))

(defn generate [target n]
  (loop [n n i 0]
    (if (= i target)
      n
      (recur (process n) (inc i)))))

(defn part-1 [input]
  (->> (parse-input input)
       (pmap (partial generate 2000))
       (reduce +)))

(comment
  (= 18261820068 (part-1 input)))

