(ns aoc-clj.2024.07.solution
  (:require [clojure.string :as str]
            [aoc-clj.utils :as utils]))

(def input (slurp "src/aoc_clj/2024/07/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/07/test-input.txt"))

(defn parse-line [line]
  (let [[left right] (str/split line #": ")]
    [(parse-long left) (mapv parse-long (str/split right #" "))]))

(defn parse-input [input]
  (let [lines (str/split input #"\n")]
    (map parse-line lines)))

(defn ops-sequences [nums]
  (utils/binary-sequences (count nums) + *))

(defn ||
  ([] 0)
  ([a] a)
  ([a b]
   (parse-long (str a b))))

(defn ops-sequences-2 [nums]
  (utils/binary-sequences-2 (count nums) + * ||))

(defn solveable? [[goal nums]]
  ; TODO we do not need to map out an operator for the first number
  (let [ops-list (ops-sequences nums)
        op-num-pairs (map (partial map vector nums) ops-list)
        results (map
                 (fn [op-nums]
                   (reduce
                    (fn [acc [num op]] (op acc num))
                    (let [[num op] (first op-nums)] (op num))
                    (rest op-nums)))
                 op-num-pairs)]
    (-> (filter #{goal} results)
        distinct)))

(defn solveable-2? [[goal nums]]
  ; we do not need to map out an operator for the first number
  (let [ops-list (ops-sequences-2 nums)
        op-num-pairs (map (partial map vector nums) ops-list)
        results (map
                 (fn [op-nums]
                   (reduce
                    (fn [acc [num op]] (op acc num))
                    (let [[num op] (first op-nums)] (op num))
                    (rest op-nums)))
                 op-num-pairs)]
    (-> (filter #{goal} results)
        distinct)))

(defn part-1 [input]
  (->> (parse-input input)
       (map solveable?)
       flatten
       (reduce +)))

(defn part-2 [input]
  (->> (parse-input input)
       (map solveable-2?)
       flatten
       (reduce +)))

(comment
  (= 465126289353 (part-1 input))
  (= 70597497486371 (part-2 input)))

