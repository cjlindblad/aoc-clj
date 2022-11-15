(ns aoc-clj.2019.06.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split-lines (slurp "src/aoc_clj/2019/06/input.txt")))

(def test-input (s/split-lines (slurp "src/aoc_clj/2019/06/test-input.txt")))

(defn parse-instruction [line]
  (let [[to from] (s/split line #"\)")]
    [(keyword from) (keyword to)]))

(defn build-graph [graph [from to]]
  (assoc graph from to))

(defn root-length
  ([graph node] (root-length graph node 0))
  ([graph node length]
   (let [next-node (graph node)]
     (if-not next-node length
             (recur graph next-node (inc length))))))

(defn all-root-lengths [graph]
  (let [nodes (keys graph)]
    (->> (map (fn [node] (root-length graph node)) nodes)
         (apply +))))

(defn part-1 [input]
  (->> input
       (map parse-instruction)
       (reduce build-graph {})
       all-root-lengths))

(defn root-path
  ([graph node] (root-path graph node [node]))
  ([graph node path]
   (let [next-node (graph node)]
     (if-not next-node path
             (recur graph next-node (conj path next-node))))))

(defn part-2 [input]
  (let [graph (->> input
                   (map parse-instruction)
                   (reduce build-graph {}))
        you-node (graph :YOU)
        santa-node (graph :SAN)
        you-path-set (apply hash-set (root-path graph you-node))
        santa-path-set (apply hash-set (root-path graph santa-node))]
    (count (set/difference (set/union you-path-set santa-path-set) (set/intersection you-path-set santa-path-set)))))

(comment
  (= 119831 (part-1 input))
  (= 322 (part-2 input)))
