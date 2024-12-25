(ns aoc-clj.2024.23.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/aoc_clj/2024/23/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/23/test-input.txt"))

(defn build-graph-data [input]
  (let [lines (str/split input #"\n")
        from-tos (map #(str/split % #"-") lines)
        connections (reduce
                     (fn [result [from to]]
                       (-> (update result from (fn [old] (set/union old #{to})))
                           (update to (fn [old] (set/union old #{from})))))
                     {}
                     from-tos)]
    connections))

(defn find-triangle [connections from]
  (-> (for [neighbour (connections from)
            neighbours-neighbour (connections neighbour)
            :when (and (not= from neighbours-neighbour)
                       ((connections neighbours-neighbour) from))]
        #{from neighbour neighbours-neighbour})
      distinct))

(defn part-1 [input]
  (let [connections (build-graph-data input)]
    (->> (keys connections)
         (filter #(re-seq #"^t.*" %))
         (mapcat (partial find-triangle connections))
         distinct
         count)))

(comment
  (= 1215 (part-1 input)))

