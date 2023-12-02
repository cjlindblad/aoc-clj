(ns aoc-clj.2023.02.solution
  (:require [clojure.string :as str]))

(def input (str/split (slurp "src/aoc_clj/2023/02/input.txt") #"\n"))

(defn parse-input-line [input-line]
  (let [[_ game-id cubes] (re-find #"Game (\d+): (.+)" input-line)
        reds (map (comp read-string last) (re-seq #"(\d+) red" cubes))
        greens (map (comp read-string last) (re-seq #"(\d+) green" cubes))
        blues (map (comp read-string last) (re-seq #"(\d+) blue" cubes))]
    {:id (read-string game-id)
     :max-red (apply max reds)
     :max-green (apply max greens)
     :max-blue (apply max blues)}))

(defn valid? [{:keys [max-red max-green max-blue]}]
  (and (<= max-red 12) (<= max-green 13) (<= max-blue 14)))

(defn part-1 [input]
  (->> (map parse-input-line input)
       (filter valid?)
       (map :id)
       (apply +)))

(defn part-2 [input]
  (->> (map parse-input-line input)
       (map (juxt :max-red :max-green :max-blue))
       (map (partial apply *))
       (apply +)))

(comment
  (= 2593 (part-1 input))
  (= 54699 (part-2 input)))
