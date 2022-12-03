(ns aoc-clj.2022.03.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/03/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/03/test-input.txt")))

(defn shared-item-types [content]
  (let [[comp-1 comp-2] (partition (/ (count content) 2) content)]
    (first (set/intersection (set comp-1) (set comp-2)))))

(def letter-values
  (zipmap (map char (concat (range (int \a) (inc (int \z))) (range (int \A) (inc (int \Z)))))
          (range 1 53)))

(defn part-1 [input]
  (->> (map (comp letter-values shared-item-types) input)
       (reduce +)))

(defn part-2 [input]
  (->> (map set input)
       (partition 3)
       (mapcat #(apply set/intersection %))
       (map letter-values)
       (reduce +)))

(comment
  (= 7848 (part-1 input))
  (= 2616 (part-2 input)))
