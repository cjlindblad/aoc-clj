(ns aoc-clj.2025.04.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/aoc_clj/2025/04/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/04/test-input.txt"))

(defn parse-input [input]
  (->>
   (let [lines (str/split input #"\n")
         width (count (first lines))
         height (count lines)]
     (for [x (range width)
           y (range height)
           :let [thing (-> (nth lines y)
                           (nth x))]
           :when (= \@ thing)]
       [x y]))
   (into #{})))

(defn get-neighbour-coords [[x y]]
  (for [xs (range (dec x) (inc (inc x)))
        ys (range (dec y) (inc (inc y)))
        :when (not= [xs ys] [x y])]
    [xs ys]))

(defn solver-1 [input]
  (let [paper-coords (parse-input input)]
    (->>
     (filter
      (fn [[x y]]
        (let [neighbour-coords (get-neighbour-coords [x y])
              paper-roll-neighbours (filter paper-coords neighbour-coords)]
          (< (count paper-roll-neighbours) 4)))
      paper-coords)
     count)))

(defn solver-2
  ([input] (solver-2 (parse-input input) 0 true))
  ([paper-coords removed-paper-rolls did-remove-rolls]
   (if (false? did-remove-rolls)
     removed-paper-rolls
     (let [removable-paper-rolls (->> (filter
                                       (fn [[x y]]
                                         (let [neighbour-coords (get-neighbour-coords [x y])
                                               paper-roll-neighbours (filter paper-coords neighbour-coords)]
                                           (< (count paper-roll-neighbours) 4)))
                                       paper-coords)
                                      (into #{}))]
       (recur
        (set/difference paper-coords removable-paper-rolls)
        (+ removed-paper-rolls (count removable-paper-rolls))
        (> (count removable-paper-rolls) 0))))))

(comment
  (= 1393 (solver-1 input))
  (= 8643 (solver-2 input)))
