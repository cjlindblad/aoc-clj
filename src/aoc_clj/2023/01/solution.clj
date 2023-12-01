(ns aoc-clj.2023.01.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2023/01/input.txt"))

(defn is-digit? [char]
  (<= (int \0) (int char) (int \9)))

(def digit-name->digit
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(def digit-regex (re-pattern (str "(?=(" (str/join "|" (conj (keys digit-name->digit) "\\d")) "))")))

(defn part-1-solver [input-lines]
  (map (partial filter is-digit?) input-lines))

(defn part-2-solver [input-lines]
  (->> input-lines
       (map (comp
             (partial map #(digit-name->digit % %))
             (partial map last)
             (partial re-seq digit-regex)))))

(defn solver [input solver-fn]
  (->> (str/split input #"\n")
       solver-fn
       (map (comp
             #(Integer/parseInt %)
             (fn [digits] (str (first digits) (last digits)))))
       (reduce +)))

(defn part-1 [input] (solver input part-1-solver))
(defn part-2 [input] (solver input part-2-solver))

(comment
  (= 55090 (part-1 input))
  (= 54845 (part-2 input)))
