(ns aoc-clj.2017.05.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (->> (slurp "src/aoc_clj/2017/05/input.txt")
                (s/split-lines)
                (map parse-int)
                (apply vector)))

(def test-input
  [0
   3
   0
   1
   -3])

input
test-input

(defn update-index [ns index f]
  (assoc ns index (f (get ns index))))

(defn out-of-bounds [ns index]
  (or
   (< index 0)
   (<= (count ns) index)))

(defn part-1-update-function [_]
  inc)

(defn part-2-update-function [offset]
  (if (>= offset 3) dec
      inc))

(defn walk-instructions
  ([instructions update-function] (walk-instructions  instructions update-function 0 0))
  ([instructions update-function index steps]
   (let [offset (get instructions index)
         next-index (+ index offset)]
     (if (out-of-bounds instructions next-index) (inc steps)
         (let [next-instructions (update-index instructions index (update-function offset))]
           (recur next-instructions update-function next-index (inc steps)))))))

(defn part-1 [input]
  (walk-instructions input part-1-update-function))

(defn part-2 [input]
  (walk-instructions input part-2-update-function))

(comment
  (= 336905 (part-1 input))
  (= 21985262 (part-2 input)))
