(ns aoc-clj.2021.02.solution
  (:require [clojure.string :as s]))

(def input
  (->> (slurp "src/aoc_clj/2021/02/input.txt")
       s/split-lines
       (map #(s/split % #" "))
       (map (fn [[direction value]] [(keyword direction) (Integer/parseInt value)]))))

(defn group-input [input]
  (->> (group-by first input)
       (mapcat (fn [[k v]] [k (reduce + (map last v))]))
       (apply hash-map)))

(defn part-1 [input]
  (let [grouped-input (group-input input)]
    (* (:forward grouped-input)
       (- (:down grouped-input) (:up grouped-input)))))

(defn handle-input [[aim horizontal depth] [type value]]
  (case type
    :down [(+ aim value) horizontal depth]
    :up [(- aim value) horizontal depth]
    :forward [aim (+ horizontal value) (+ depth (* aim value))]))

(defn part-2 [input]
  (->> (reduce handle-input [0 0 0] input)
       (drop 1)
       (apply *)))

(comment
  (= 1893605 (part-1 input))
  (= 2120734350 (part-2 input)))
