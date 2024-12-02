(ns aoc-clj.2024.02.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/02/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/02/test-input.txt"))

(defn valid-diffs? [report]
  (->> (partition 2 1 report)
       (map (partial apply -))
       (map #(Math/abs %))
       (remove #{1 2 3})
       count
       (= 0)))

(defn safe? [report]
  (and
   (or (apply < report) (apply > report))
   (valid-diffs? report)))

(defn part-1 [input]
  (->> (str/split input #"\n")
       (mapv #(read-string (str "[" % "]")))
       (filter safe?)
       count))

(defn problem-dampen [report]
  (for [i (range (count report))]
    (concat (take i report)
            (take (- (count report) i) (drop (inc i) report)))))

(defn safe-2? [report]
  (let [variants (problem-dampen report)]
    (pos?
     (count
      (filter #(and
                (or (apply < %) (apply > %))
                (valid-diffs? %)) variants)))))

(defn part-2 [input]
  (->> (str/split input #"\n")
       (mapv #(read-string (str "[" % "]")))
       (filter safe-2?)
       count))

(comment
  (= 598 (part-1 input))
  (= 634 (part-2 input)))
