(ns aoc-clj.2020.02.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2020/02/input.txt")))

(defn parse-int [n] (Integer/parseInt n))

(defn parse-input-line [line]
  (let [[range letter password] (s/split line #" ")
        int-range (map parse-int (s/split range #"-"))]
    {:min-range (first int-range)
     :max-range (last int-range)
     :letter (first (drop-last letter))
     :password password}))

(defn occurences [ns n]
  (->> (filter #(= n %) ns)
       count))

(defn validate-password [{:keys [min-range max-range letter password]}]
  (let [letter-count (occurences password letter)]
    (and (<= min-range letter-count)
         (>= max-range letter-count))))

(defn part-1 [input]
  (->> input
       (map parse-input-line)
       (map validate-password)
       (filter identity)
       count))

(defn validate-password-2 [{:keys [min-range max-range letter password]}]
  (let [pos-1 (nth password (dec min-range))
        pos-2 (nth password (dec max-range))]
    (= 1 (occurences [pos-1 pos-2] letter))))

(defn part-2 [input]
  (->> input
       (map parse-input-line)
       (map validate-password-2)
       (filter identity)
       count))

(comment
  (= 546 (part-1 input))
  (= 275 (part-2 input)))
