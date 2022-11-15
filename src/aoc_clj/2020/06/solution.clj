(ns aoc-clj.2020.06.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (slurp "src/aoc_clj/2020/06/input.txt"))

(defn parse-input [input]
  (let [raw-groups (s/split input #"\n\n")
        groups (map #(s/split % #"\n") raw-groups)]
    groups))

(defn yeses
  ([group f] (yeses (rest group) (apply hash-set (first group)) f))
  ([group answered f]
   (if-not (seq group) answered
           (recur (rest group) (f answered (apply hash-set (first group))) f))))

(defn any-yes [group] (yeses group set/union))
(defn every-yes [group] (yeses group set/intersection))

(defn solver [f input]
  (->> (parse-input input)
       (map f)
       (map count)
       (reduce +)))

(def part-1 (partial solver any-yes))
(def part-2 (partial solver every-yes))

(comment
  (= 6930 (part-1 input))
  (= 3585 (part-2 input)))
