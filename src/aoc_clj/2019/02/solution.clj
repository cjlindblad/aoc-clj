(ns aoc-clj.2019.02.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (mapv (comp parse-int s/trim) (s/split (slurp "src/aoc_clj/2019/02/input.txt") #",")))

(defn instruction [f memory address]
  (let [a (get memory (get memory (+ 1 address)))
        b (get memory (get memory (+ 2 address)))]
    (assoc memory (get memory (+ 3 address)) (f a b))))

(def add (partial instruction +))

(def mult (partial instruction *))

(defn execute [memory address]
  (let [opcode (get memory address)]
    (case opcode
      99 memory
      1 (recur (add memory address) (+ 4 address))
      2 (recur (mult memory address) (+ 4 address)))))

(defn program [noun verb input]
  (-> input
      (assoc 1 noun)
      (assoc 2 verb)
      (execute 0)
      first))

(def part-1 (partial program 12 2))

(defn part-2 [input]
  (first
   (for [noun (range 0 100)
         verb (range 0 100)
         :when (= 19690720 (program noun verb input))]
     (+ verb (* 100 noun)))))

(comment
  (= 5110675 (part-1 input))
  (= 4847 (part-2 input)))
