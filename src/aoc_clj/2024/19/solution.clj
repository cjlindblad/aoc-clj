(ns aoc-clj.2024.19.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/19/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/19/test-input.txt"))

(defn parse-input [input]
  (let [[towels designs] (str/split input #"\n\n")]
    {:towels (str/split towels #", ")
     :designs (str/split designs #"\n")}))

(defn buildable? [dictionary towel]
  (let [n (count towel)
        dp-atom (atom (vec (concat [true] (repeat n false))))]
    (doseq [i (range 1 (inc n))]
      (doseq [j (range i)]
        (let [substring (subs towel j i)]
          (when (and (@dp-atom j)
                     (dictionary substring))
            (swap! dp-atom assoc i true)))))
    (@dp-atom n)))

(defn part-1 [input]
  (let [{:keys [towels designs]} (parse-input input)
        dictionary (into #{} towels)]
    (->> (filter (fn [towel] (buildable? dictionary towel)) designs)
         count)))

(part-1 input)
