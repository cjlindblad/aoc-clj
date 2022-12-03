(ns aoc-clj.2022.03.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/03/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/03/test-input.txt")))

(defn share-item-types [content]
  (let [[comp-1 comp-2] (partition (/ (count content) 2) content)]
    (first (set/intersection (into #{} comp-1) (into #{} comp-2)))))

(def letter-values
  (merge (zipmap (map char (range 97 123)) (range 1 27))
         (zipmap (map char (range 65 91)) (range 27 53))))

(defn part-1 [input]
  (->> (map (comp letter-values share-item-types) input)
       (reduce +)))

(defn group->sets [group] (map #(apply hash-set %) group))
(defn common-item [sets] (first (apply set/intersection sets)))

(defn part-2 [input]
  (->> (partition 3 input)
       (map (comp letter-values common-item group->sets))
       (reduce +)))

(comment
  (= 7848 (part-1 input))
  (= 2616 (part-2 input)))
