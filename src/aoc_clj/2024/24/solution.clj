(ns aoc-clj.2024.24.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/24/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/24/test-input.txt"))
(def larger-test-input (slurp "src/aoc_clj/2024/24/larger-test-input.txt"))

(defn parse-parents-rule [gate-line]
  (let [[_ gate-1 rule gate-2 gate-out] (first (re-seq #"(.+) (.+) (.+) \-\> (.+)" gate-line))]
    {gate-out [gate-1 rule gate-2]}))

(defn parse-start-val [line]
  (let [[_ node value] (first (re-seq #"(.+)\: (\d+)" line))]
    [node (read-string value)]))

(defn solve [node start-vals parents-rules]
  (if (start-vals node)
    (start-vals node)
    (let [[left-node rule right-node] (parents-rules node)
          left (solve left-node start-vals parents-rules)
          right (solve right-node start-vals parents-rules)]
      (case rule
        "AND" (bit-and left right)
        "OR" (bit-or left right)
        "XOR" (bit-xor left right)))))

(def solve-memo (memoize solve))

(defn part-1 [input]
  (let [[start-vals-lines gates-lines] (map (fn [x] (str/split x #"\n")) (str/split input #"\n\n"))
        start-vals-data (map parse-start-val start-vals-lines)
        start-vals (into {} start-vals-data)
        parents-data (map parse-parents-rule gates-lines)
        parents-rules (reduce (fn [acc cur] (merge acc cur)) {} parents-data)
        goal-nodes (reverse (sort (filter (fn [node] (re-seq #"^z.*" node)) (keys parents-rules))))
        goal-vals (map (fn [goal-node] (solve-memo goal-node start-vals parents-rules)) goal-nodes)]
    (read-string (str "2r" (apply str goal-vals)))))

(comment
  (= 57344080719736 (part-1 input)))
