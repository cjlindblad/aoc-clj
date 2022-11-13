(ns aoc-clj.2020.05.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split-lines (slurp "src/aoc_clj/2020/05/input.txt")))

(defn parse-char [c]
  (case c
    \F :lower
    \B :upper
    \L :lower
    \R :upper))

(defn parse-line [line]
  {:rows (map parse-char (take 7 line))
   :cols (map parse-char (drop 7 line))})

(defn lower-half [xs]
  (let [length (count xs)
        half-length (/ length 2)]
    (drop-last half-length xs)))

(defn upper-half [xs]
  (let [length (count xs)
        half-length (/ length 2)]
    (drop half-length xs)))

(defn find-target [range space]
  (case space
    :upper (upper-half range)
    :lower (lower-half range)))

(def row-range (range 0 128))
(def col-range (range 0 8))

(defn find-seat [{:keys [rows cols]}]
  [(first (reduce find-target row-range rows))
   (first (reduce find-target col-range cols))])

(defn seat-id [[row col]]
  (+ (* 8 row) col))

(defn part-1 [input]
  (->> input
       (map parse-line)
       (map find-seat)
       (map seat-id)
       sort
       last))

(defn find-missing-seat [seat-ids]
  (let [min-seat (apply min seat-ids)
        max-seat (apply max seat-ids)
        existing-seats-set (apply hash-set seat-ids)
        all-seats-set (apply hash-set (range min-seat (inc max-seat)))]
    (set/difference all-seats-set existing-seats-set)))

(defn part-2 [input]
  (->> input
       (map parse-line)
       (map find-seat)
       (map seat-id)
       find-missing-seat
       first))

(comment
  (= 944 (part-1 input))
  (= 554 (part-2 input)))

