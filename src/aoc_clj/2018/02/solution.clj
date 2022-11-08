(ns aoc-clj.2018.02.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2018/02/input.txt")))

(defn exactly-n-of [xs n]
  (->> (group-by identity xs)
       (filter (fn [[k v]] (= n (count v))))
       (map (fn [[k v]] (str k)))
       first))

(defn part-1 [input]
  (let [twos (filter (complement nil?) (map #(exactly-n-of % 2) input))
        threes (filter (complement nil?) (map #(exactly-n-of % 3) input))]
    [twos threes]
    (* (count twos) (count threes))))

(defn distance [a b]
  (->> (map = a b)
       (filter (complement identity))
       count))

(defn seq-union [a b]
  (->> (map #(vector (= %1 %2) %2) a b)
       (filter first)
       (map last)
       (reduce str)))

(defn part-2 [input]
  (->> (for [a input
             b input
             :when (= 1 (distance a b))]
         [a b])
       first
       (apply seq-union)))

(comment
  (= 5390 (part-1 input))
  (= "nvosmkcdtdbfhyxsphzgraljq" (part-2 input)))
