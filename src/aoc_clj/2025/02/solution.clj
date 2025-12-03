(ns aoc-clj.2025.02.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2025/02/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/02/test-input.txt"))

(defn parse-input [input]
  (let [ranges (-> (str/trim input)
                   (str/split #","))]
    (->> (map #(str/split % #"-") ranges)
         (mapv (partial mapv #(Long/parseLong %))))))

(defn invalid? [id]
  (let [stringified (str id)
        len (count stringified)]
    (if (odd? len)
      false
      (let [[first second] (partition (/ len 2) stringified)]
        (= first second)))))

(defn invalid-in-range [[from to]]
  (filter invalid? (range from (inc to)))
  )

(defn solver-1 [input]
  (let [ranges (parse-input input)]
    (->> (mapcat invalid-in-range ranges)
         (reduce +))))

(comment
  (= 19219508902 (solver-1 input)))
