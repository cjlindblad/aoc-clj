(ns aoc-clj.2025.06.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2025/06/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/06/test-input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map #(str/split % #"\s+"))
       (map (partial filter #(< 0 (count %))))
       (apply mapv vector)
       (map
        (fn [xs]
          {:numbers (mapv #(Long/parseLong %) (drop-last 1 xs))
           :operation (resolve (symbol (first (take-last 1 xs))))}))))

(defn solver-1 [input]
  (->> (parse-input input)
       (map (fn [{:keys [numbers operation]}] (apply operation numbers)))
       (reduce +)))

(defn solver-2 [input]
  (let [lines (str/split input #"\n")
        raw-numbers (drop-last 1 lines)
        raw-operations (first (take-last 1 lines))
        operations (map (comp resolve symbol last) (re-seq #"([\*|\+])" raw-operations))
        numbers (->> (apply mapv vector raw-numbers)
                     (map (partial filter (complement #{\space})))
                     (partition-by empty?)
                     (filter (complement #{'(())}))
                     (map (partial map (partial apply str)))
                     (map (partial map #(Long/parseLong %))))]
    (->> (mapv apply operations numbers)
         (reduce +))))

(comment
  (= 4583860641327 (solver-1 input))
  (= 11602774058280 (solver-2 input)))

