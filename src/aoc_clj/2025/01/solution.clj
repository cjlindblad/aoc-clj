(ns aoc-clj.2025.01.solution
  (:require [clojure.string :as str]))

(def test-input (slurp "src/aoc_clj/2025/01/test-input.txt"))
(def input (slurp "src/aoc_clj/2025/01/input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (mapcat #(re-seq #"([L|R])(\d+)" %))
       (map (partial drop 1))
       (map (fn [[dir amount]] [(keyword dir) (Long/parseLong amount)]))))

(defn rotate [dial [direction amount]]
  (let [delta (if (= :L direction) (- amount) amount)]
    (mod (+ dial delta) 100)))

(defn solver-1 [input]
  (->> (reductions
        rotate
        50
        (parse-input input))
       (filter #{0})
       count))

(defn rotations [dials [direction amount]]
  (->> (reductions
        (fn [result step] (println result) (rotate result [direction step]))
        (last dials)
        (repeat amount 1))
       (drop 1)))

(defn solver-2 [input]
  (->> (reductions
        rotations
        [50]
        (parse-input input))
       #_(filter #{0})
       #_count))

(solver-2 test-input)

(comment
  (= 1031 (solver-1 input)))

