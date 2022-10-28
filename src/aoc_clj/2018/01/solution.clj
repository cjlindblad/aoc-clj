(ns aoc-clj.2018.01.solution
  (:require [clojure.string :as s]
            [aoc-clj.utils :as u]))

(def input (s/split (slurp "src/aoc_clj/2018/01/input.txt") #"\n"))

(defn parse-input [input-line]
  (let [operator (symbol (str (first input-line)))
        value (Integer/parseInt (apply str (rest input-line)))]
    (list operator value)))

(defn part-1 [input]
  (->> input
       (map parse-input)
       (map eval)
       (apply +)))

(defn changes->frequencies [frequencies change]
  (if (empty? frequencies) [change]
      (conj frequencies (+ (last frequencies) change))))

(defn part-2 [input]
  (->> input
       (map parse-input)
       (map eval)
       cycle
       (reductions +)
       u/first-duplicate
       ))

(comment
  (= 439 (part-1 input))
  (= 124645 (part-2 input)))
