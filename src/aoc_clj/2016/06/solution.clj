(ns aoc-clj.2016.06.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2016/06/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2016/06/test-input.txt")))

(defn transpose [xs]
  (apply map vector xs))

(defn most-common [xs]
  (->> (group-by identity xs)
       (sort-by #(count (last %)))
       last
       first
       ))

(defn least-common [xs]
  (->> (group-by identity xs)
       (sort-by #(count (last %)))
       ffirst))

(defn part-1 [input]
  (->> (transpose input)
       (map most-common)
       (apply str)))

(defn part-2 [input]
  (->> (transpose input)
       (map least-common)
       (apply str)))

(comment
  (= "wkbvmikb" (part-1 input))
  (= "evakwaga" (part-2 input)))
