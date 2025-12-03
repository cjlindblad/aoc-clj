(ns aoc-clj.2025.03.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2025/03/input.txt"))
(def test-input (slurp "src/aoc_clj/2025/03/test-input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map (partial mapv #(Character/digit % 10)))))

(defn find-batteries
  ([batteries bank] (find-batteries batteries bank []))
  ([batteries-left bank result]
   (if (zero? batteries-left)
     (Long/parseLong (apply str result))
     (let [next-largest (->> (drop-last (dec batteries-left) bank)
                             sort
                             reverse
                             first)
           next-largest-index (.indexOf bank next-largest)]
       (recur
        (dec batteries-left)
        (drop (inc next-largest-index) bank)
        (conj result next-largest))))))

(defn solver [batteries input]
  (->> (parse-input input)
       (map (partial find-batteries batteries))
       (reduce +)))

(def solver-1 (partial solver 2))
(def solver-2 (partial solver 12))

(comment
  (= 17229 (solver-1 input))
  (= 170520923035051 (solver-2 input)))

