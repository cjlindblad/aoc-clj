(ns aoc-clj.2024.04.solution
  (:require [clojure.string :as str]))

(def input (str/split (slurp "src/aoc_clj/2024/04/input.txt") #"\n"))
(def test-input (str/split (slurp "src/aoc_clj/2024/04/test-input.txt") #"\n"))

(defn in-bounds [size]
  (fn [[x y]] (and (<= 0 x) (<= 0 y) (< x size) (< y size))))

(defn get-coord-ranges [[x y] size]
  (let [deltas [[[0 0] [1 0] [2 0] [3 0]]
                [[0 0] [-1 0] [-2 0] [-3 0]]
                [[0 0] [0 1] [0 2] [0 3]]
                [[0 0] [0 -1] [0 -2] [0 -3]]
                [[0 0] [1 1] [2 2] [3 3]]
                [[0 0] [-1 1] [-2 2] [-3 3]]
                [[0 0] [-1 -1] [-2 -2] [-3 -3]]
                [[0 0] [1 -1] [2 -2] [3 -3]]]]
    (->> (map (partial map (fn [[dx dy]] [(+ dx x) (+ dy y)])) deltas)
         (map (partial filter (in-bounds size)))
         (filter #(= 4 (count %))))))

(defn get-coord-ranges-2 [[x y] size]
  (->> [[0 0] [-1 -1] [1 -1] [-1 1] [1 1]]
       (map (fn [[dx dy]] [(+ dx x) (+ dy y)]))
       (filter (in-bounds size))))

(defn get-word [coord-range input]
  (->> (map (fn [[x y]] (get-in input [y x])) coord-range)
       (apply str)))

(defn part-1 [input]
  (let [size (count input)
        coords (for [x (range 0 size) y (range 0 size)] [x y])
        coord-ranges (map #(get-coord-ranges % size) coords)
        words (mapcat (partial map #(get-word % input)) coord-ranges)
        xmases (filter #{"XMAS"} words)]
    (count xmases)))

(defn part-2 [input]
  (let [size (count input)
        coords (for [x (range 0 size) y (range 0 size)] [x y])
        coord-ranges (map #(get-coord-ranges-2 % size) coords)
        words (map #(get-word % input) coord-ranges)
        x-mases (filter #{"ASSMM" "ASMSM" "AMSMS" "AMMSS"} words)]
    (count x-mases)))

(comment
  (= 2654 (part-1 input))
  (= 1990 (part-2 input)))
