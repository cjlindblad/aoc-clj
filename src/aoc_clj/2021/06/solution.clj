(ns aoc-clj.2021.06.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (slurp "src/aoc_clj/2021/06/input.txt"))

(defn parse-fish [input]
  (let [ns (->> (s/split (s/trim input) #",") (map parse-int))
        group (group-by identity ns)]
    (zipmap (keys group) (map count (vals group)))))

(defn next-day [fish]
  (let [zeroes (get fish 0 0)
        ones (get fish 1 0)
        twos (get fish 2 0)
        threes (get fish 3 0)
        fours (get fish 4 0)
        fives (get fish 5 0)
        sixes (get fish 6 0)
        sevens (get fish 7 0)
        eights (get fish 8 0)]
    {8 zeroes
     7 eights
     6 (+ sevens zeroes)
     5 sixes
     4 fives
     3 fours
     2 threes
     1 twos
     0 ones}))

(defn count-fish [days input]
  (let [fish (parse-fish input)
        days (range 0 days)
        final-fish (reduce (fn [fish _] (next-day fish)) fish days)]
    (reduce + (vals final-fish))))

(def part-1 (partial count-fish 80))
(def part-2 (partial count-fish 256))

(comment
  (= 379114 (part-1 input))
  (= 1702631502303 (part-2 input)))
