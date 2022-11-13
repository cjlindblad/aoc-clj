(ns aoc-clj.2015.05.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2015/05/input.txt")))

(defn vowels [s]
  (filter #{\a \e \i \o \u} s))

(defn has-double-letter? [s]
  (let [pairs (partition 2 1 s)]
    (some (fn [[a b]] (= a b)) pairs)))

(defn free-from [s naughty]
  (= 0 (count (filter #(s/includes? s %) naughty))))

(def naughty-strings ["ab" "cd" "pq" "xy"])

(defn nice? [s]
  (and
   (<= 3 (count (vowels s)))
   (has-double-letter? s)
   (free-from s naughty-strings)))

(defn part-1 [input]
  (->> input
       (filter nice?)
       count))

(defn part-2-rule-1 [s]
  (re-seq #"([a-z])([a-z]).*\1\2" s))

(defn part-2-rule-2 [s]
  (re-seq #"([a-z]).\1" s))

(defn nice-2? [s]
  (and
   (part-2-rule-1 s)
   (part-2-rule-2 s)))

(defn part-2 [input]
  (->> input
       (filter nice-2?)
       count))

(comment
  (= 258 (part-1 input))
  (= 53 (part-2 input)))
