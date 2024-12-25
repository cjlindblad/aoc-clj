(ns aoc-clj.2024.25.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/aoc_clj/2024/25/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/25/test-input.txt"))

(defn parse-schematic [lines]
  (->> (for [y (range (count lines))
             x (range (count (first lines)))
             :when (#{\#} (get-in lines [y x]))]
         [x y])
       (into #{})))

(defn part-1 [input]
  (let [schematic-lines (map #(str/split % #"\n") (str/split input #"\n\n"))
        schematics (map parse-schematic schematic-lines)
        locks (filter (partial some #{[0 0]}) schematics)
        lock-keys (filter (partial some #{[0 6]}) schematics)]
    (->> (for [lock-key lock-keys
               lock locks
               :when (zero? (count (set/intersection lock-key lock)))]
           1)
         (reduce +))))
key

(comment
  (= 3608 (part-1 input)))

