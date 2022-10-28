(ns aoc-clj.2021.01.solution
  (:require [clojure.string :as s]))

(defn parse-input [n] (Integer/parseInt n))

(def input (map parse-input (s/split-lines (slurp "src/aoc_clj/2021/01/input.txt"))))

(defn filter-increasing [ns]
  (->> ns
       (map vector (drop 1 ns))
       (filter #(apply > %))
       (map first)))

(defn part-1 [input]
  (->> (filter-increasing input)
       count))

(defn sliding-window [ns size]
  (->> (partition size 1 ns)
       (map #(apply + %))))

(defn part-2 [input]
  (->> (sliding-window input 3)
       filter-increasing
       count))

(comment
  (= 1676 (part-1 input))
  (= 1706 (part-2 input)))

