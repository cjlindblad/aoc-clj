(ns aoc-clj.2024.19.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/19/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/19/test-input.txt"))

(defn parse-input [input]
  (let [[towels designs] (str/split input #"\n\n")]
    {:towels (str/split towels #", ")
     :designs (str/split designs #"\n")}))

(defn word-break [s word-set]
  (let [n (count s)
        dp (atom (vec (repeat (inc n) 0)))]
    (swap! dp assoc 0 1)
    (doseq [i (range 1 (inc n))]
      (doseq [j (range i)]
        (when (word-set (subs s j i))
          (let [old (@dp i)
                add (@dp j)]
            (swap! dp assoc i (+ old add))))))
    (@dp n)))

(defn solver [input]
  (let [{:keys [towels designs]} (parse-input input)
        word-set (into #{} towels)]
    (->> (map (fn [design] (word-break design word-set)) designs)
         (filter pos?))))

(defn part-1 [input] (count (solver input)))

(defn part-2 [input] (reduce + (solver input)))

(comment
  (= 209 (part-1 input))
  (= 777669668613191 (part-2 input)))
