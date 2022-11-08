(ns aoc-clj.2017.02.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input
  (->> (slurp "src/aoc_clj/2017/02/input.txt")
       (s/split-lines)
       (map #(s/split % #"\s"))
       (map #(map parse-int %))))

(defn calculate-checksum [ns]
  (let [smallest (apply min ns)
        largest (apply max ns)]
    (- largest smallest)))

(defn part-1 [input]
  (->> input
       (map calculate-checksum)
       (reduce +)))

(defn calculate-checksum-2 [ns]
  (first
   (for [x ns
         y ns
         :when (and (not= x y) (= 0 (mod x y)))]
     (/ x y))))

(defn part-2 [input]
  (->> input
       (map calculate-checksum-2)
       (reduce +)))

(comment
  (= 47623 (part-1 input))
  (= 312 (part-2 input)))
