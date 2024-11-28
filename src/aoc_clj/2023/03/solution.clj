(ns aoc-clj.2023.03.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2023/03/input.txt"))
(defn parse-input [input]
  (->> (str/split input #"\n")
       (map #(re-seq #"\d+" %))
       (map (partial map parse-long))
       (apply interleave)
       (partition 2)))

(defn parse-input-2 [input]
  (->> (str/split input #"\n")
       (map #(re-seq #"\d+" %))
       (map (partial apply str))
       (map parse-long)
       (conj [])))

(defn distances [time]
  (for [speed (range (inc time))]
    (* speed (- time speed))))

(defn win-options [[time record]]
  (filter (partial < record) (distances time)))

(defn solver [input]
  (->>
   (map win-options input)
   (map count)
   (apply *)))

(defn part-1 [input]
  (solver (parse-input input)))

(defn part-2 [input]
  (solver (parse-input-2 input)))

(comment
  (= 2374848 (part-1 input))
  (= 39132886 (part-2 input)))
