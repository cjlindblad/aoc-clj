(ns aoc-clj.2022.09.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/09/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/09/test-input.txt")))

(defn parse-instruction [line]
  (let [[direction distance] (string/split line #" ")]
    (repeat (Integer/parseInt distance) direction)))

(defn touching? [[head-x head-y] [tail-x tail-y]]
  (and (<= (Math/abs (- head-x tail-x)) 1)
       (<= (Math/abs (- head-y tail-y)) 1)))

(defn same-col? [[head-x _] [tail-x _]] (= head-x tail-x))
(defn same-row? [[_ head-y] [_ tail-y]] (= head-y tail-y))

(defn next-head [[head-x head-y] direction]
  (case direction
    "U" [head-x (inc head-y)]
    "R" [(inc head-x) head-y]
    "D" [head-x (dec head-y)]
    "L" [(dec head-x) head-y]))

(defn next-tail [[tail-x tail-y :as tail] [head-x head-y :as head]]
  (cond (touching? head tail) tail
        (same-row? head tail) (if (> head-x tail-x) [(inc tail-x) tail-y] [(dec tail-x) tail-y])
        (same-col? head tail) (if (> head-y tail-y) [tail-x (inc tail-y)] [tail-x (dec tail-y)])
        :else (cond (and (> head-x tail-x) (> head-y tail-y)) [(inc tail-x) (inc tail-y)]
                    (and (> head-x tail-x) (< head-y tail-y)) [(inc tail-x) (dec tail-y)]
                    (and (< head-x tail-x) (> head-y tail-y)) [(dec tail-x) (inc tail-y)]
                    (and (< head-x tail-x) (< head-y tail-y)) [(dec tail-x) (dec tail-y)])))

(defn get-tails
  ([heads limit] (get-tails heads limit []))
  ([heads limit result]
   (if (= limit (count result)) result
       (let [tails (reductions next-tail [0 0] heads)]
         (recur tails limit (conj result tails))))))

(defn solver [limit input]
  (let [instructions (mapcat parse-instruction input)
        heads (reductions next-head [0 0] instructions)
        tails (get-tails heads limit)]
    (count (set (last tails)))))

(def part-1 (partial solver 1))
(def part-2 (partial solver 9))

(comment
  (= 6067 (part-1 input))
  (= 2471 (part-2 input)))
