(ns aoc-clj.2015.02.solution
  (:require [clojure.string :as str]))

(def input (str/split-lines (slurp "src/aoc_clj/2015/02/input.txt")))
input

(defn parse-int [s] (Integer/parseInt s))

(defn parse-input [line]
  (->> (str/split line #"x")
       (map parse-int)))

(defn lengths->areas [lengths]
  (->> (cycle lengths)
       (take 4)
       (partition 2 1)
       (map (fn [[a b]] (* a b)))))

(defn required-wrapping [areas]
  (let [smallest-area (apply min areas)]
    (+ smallest-area
       (* 2 (apply + areas)))))

(defn smallest-perimeter [lengths]
  (->> (sort lengths)
       (take 2)
       (map #(* 2 %))
       (apply +)))

(defn required-ribbon [lengths]
  (let [perimeter (smallest-perimeter lengths)
        ribbon (apply * lengths)]
    (+ perimeter ribbon)))

(defn part-1 [input]
  (->> input
       (map parse-input)
       (map lengths->areas)
       (map required-wrapping)
       (apply +)))

(defn part-2 [input]
  (->> input
       (map parse-input)
       (map required-ribbon)
       (apply +)))

(comment
  (=  1606483 (part-1 input))
  (= 3842356 (part-2 input)))
