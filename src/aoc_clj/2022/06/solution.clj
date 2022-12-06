(ns aoc-clj.2022.06.solution
  (:require [clojure.string :as string]))

(def input (string/trim (slurp "src/aoc_clj/2022/06/input.txt")))

(defn solver
  ([marker-length s] (solver marker-length s 0))
  ([marker-length s char-count]
   (let [candidate (take marker-length s)]
     (if (apply distinct? candidate) (+ char-count marker-length)
         (recur marker-length (rest s) (inc char-count))))))

(def part-1 (partial solver 4))
(def part-2 (partial solver 14))

(comment
  (= 1833 (part-1 input))
  (= 3425 (part-2 input)))
