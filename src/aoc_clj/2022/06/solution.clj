(ns aoc-clj.2022.06.solution
  (:require [clojure.string :as string]))

(def input (string/trim (slurp "src/aoc_clj/2022/06/input.txt")))

(defn solver [length input]
  (->> (partition length 1 input)
       (map (comp count set))
       (take-while (complement #{length}))
       (#(+ length (count %)))))

(def part-1 (partial solver 4))
(def part-2 (partial solver 14))

(comment
  (= 1833 (part-1 input))
  (= 3425 (part-2 input)))
