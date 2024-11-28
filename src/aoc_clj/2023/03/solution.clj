(ns aoc-clj.2023.03.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2023/03/input.txt"))
(defn parse-input [input]
  (->> (str/split input #"\n")
       (map #(re-seq #"\d+" %))
       (map (partial map parse-long))
       (apply interleave)
       (partition 2)))

(defn distances [time]
  (for [speed (range (inc time))]
    (* speed (- time speed))))

(defn win-options [[time record]]
  (filter (partial < record) (distances time)))

(defn part-1 [input]
  (->> (parse-input input)
       (map win-options)
       (map count)
       (apply *)))

(comment
  (= 2374848 (part-1 input)))
