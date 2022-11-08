(ns aoc-clj.2016.02.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2016/02/input.txt")))
input

(def test-input ["ULL" "RRDDD" "LURDL" "UUUUD"])

(def moves
  {1 {:U 1
      :R 2
      :D 4
      :L 1}
   2 {:U 2
      :R 3
      :D 5
      :L 1}
   3 {:U 3
      :R 3
      :D 6
      :L 2}
   4 {:U 1
      :R 5
      :D 7
      :L 4}
   5 {:U 2
      :R 6
      :D 8
      :L 4}
   6 {:U 3
      :R 6
      :D 9
      :L 5}
   7 {:U 4
      :R 8
      :D 7
      :L 7}
   8 {:U 5
      :R 9
      :D 8
      :L 7}
   9 {:U 6
      :R 9
      :D 9
      :L 8}})

(def moves-2
  {1 {:U 1
      :R 1
      :D 3
      :L 1}
   2 {:U 2
      :R 3
      :D 6
      :L 2}
   3 {:U 1
      :R 4
      :D 7
      :L 2}
   4 {:U 4
      :R 4
      :D 8
      :L 3}
   5 {:U 5
      :R 6
      :D 5
      :L 5}
   6 {:U 2
      :R 7
      :D "A"
      :L 5}
   7 {:U 3
      :R 8
      :D "B"
      :L 6}
   8 {:U 4
      :R 9
      :D "C"
      :L 7}
   9 {:U 9
      :R 9
      :D 9
      :L 8}
   "A" {:U 6
        :R "B"
        :D "A"
        :L "A"}
   "B" {:U 7
        :R "C"
        :D "D"
        :L "A"}
   "C" {:U 8
        :R "C"
        :D "C"
        :L "B"}
   "D" {:U "B"
        :R "D"
        :D "D"
        :L "D"}})

(defn line->instructions [line]
  (->> (s/split line #"")
       (map keyword)))

(defn instructions->key [move-map instructions prev-key]
  (if-not (seq instructions) prev-key
          (recur
           move-map
           (rest instructions)
           (get-in move-map [prev-key (first instructions)]))))

(defn solver [move-map input]
  (->> input
       (map line->instructions)
       (reductions (fn [key instructions] (instructions->key move-map instructions key)) 5)
       (drop 1)
       (apply str)))

(def part-1 (partial solver moves))
(def part-2 (partial solver moves-2))

(comment
  (= "53255" (part-1 input))
  (= "7423A" (part-2 input)))

