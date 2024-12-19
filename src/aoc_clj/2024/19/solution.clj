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

(defn arrangements-recursive [dictionary remaining memo]
  (if (@memo remaining)
    (@memo remaining)
    (if-not (seq remaining)
      [""]
      (let [result (atom [])]
        (doseq [i (range 1 (inc (count remaining)))]
          (let [prefix (subs remaining 0 i)]
            (when (dictionary prefix)
              (let [suffix-ways (arrangements-recursive dictionary (subs remaining i) memo)]
                (doseq [way suffix-ways]
                  (if (= "" way)
                    (swap! result conj prefix)
                    (swap! result conj (str prefix " " way))))))))

        (swap! memo assoc remaining @result)
        @result))))

(defn arrangements [dictionary design]
  (arrangements-recursive dictionary design (atom {})))

(defn part-2 [input]
  (let [{:keys [towels designs]} (parse-input input)
        dictionary (into #{} towels)
        valid-designs (filter (fn [towel] (buildable? dictionary towel)) designs)]
    ;(arrangements dictionary (first valid-designs))
    (->> (map (fn [design] (arrangements dictionary design)) valid-designs)
         (map count)
         (reduce +))))

(part-2 input)

;(part-1 input) ; 209
