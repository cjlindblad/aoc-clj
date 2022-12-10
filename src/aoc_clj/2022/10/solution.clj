(ns aoc-clj.2022.10.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/10/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/10/test-input.txt")))

(defn parse-instruction [line]
  (let [[operation value] (string/split line #" ")]
    (case operation
      "noop" [{:op :noop}]
      "addx" [{:op :addx :val 0} {:op :addx :val (Integer/parseInt value)}])))

(defn eval-instruction [x {:keys [op val]}]
  (if (#{:noop} op) x (+ x val)))

(defn pixel [x-val cycle]
  (if (>= 1 (Math/abs (- x-val (mod (dec cycle) 40))))
    \#
    \.))

(defn solver [input]
  (let [instructions (mapcat parse-instruction input)
        x-vals (reductions eval-instruction 1 instructions)]
    {:signal-strengths (map #(* % (nth x-vals (dec %))) [20 60 100 140 180 220])
     :pixels (map pixel x-vals (range 1 (inc 240)))}))

(defn part-1 [input]
  (reduce + (:signal-strengths (solver input))))

(defn part-2 [input]
  (map (partial apply str) (partition 40 (:pixels (solver input)))))

(comment
  (= 15360 (part-1 input))
  (= (part-2 input)
     ["###..#..#.#....#..#...##..##..####..##.."
      "#..#.#..#.#....#..#....#.#..#....#.#..#."
      "#..#.####.#....####....#.#......#..#..#."
      "###..#..#.#....#..#....#.#.##..#...####."
      "#....#..#.#....#..#.#..#.#..#.#....#..#."
      "#....#..#.####.#..#..##...###.####.#..#."]))
