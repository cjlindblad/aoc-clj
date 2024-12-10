(ns aoc-clj.2024.10.solution
  (:require [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def input (slurp "src/aoc_clj/2024/10/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/10/test-input.txt"))

(defn parse-long [n]
  (Long/parseLong n))

(defn parse-lines [input]
  (->> (str/split input #"\n") (map #(str/split % #"")) (mapv (partial mapv parse-long))))

(def deltas [[0 1]
             [1 0]
             [0 -1]
             [-1 0]])

(defn neighbour-coords [[x y] max-x max-y]
  (->> (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)
       (filter (fn [[x y]] (and (<= 0 x max-x) (<= 0 y max-y))))))

(defn build-nodes [lines]
  (let [get-node (fn [[x y]] {:x x :y y :v (get-in lines [y x])})
        max-x (dec (count (first lines)))
        max-y (dec (count lines))
        coords (for [x (range (inc max-x)) y (range (inc max-y))] [x y])
        coord->neighbours (map (fn [coord] [coord (neighbour-coords coord max-x max-y)]) coords)
        nodes (map
               (fn [[coord neighbours]]
                 (let [coord-node (get-node coord)]
                   [coord-node (mapv identity (filter (fn [{:keys [v]}] (= (inc (:v coord-node)) v)) (mapv get-node neighbours)))]))
               coord->neighbours)]
    nodes))

(defn build-graph [nodes]
  (reduce
   (fn [g [node neighbours]]
     (if (empty? neighbours)
       g
       (let [pairs (for [neighbour neighbours] [node neighbour])]
         (apply graph/add-edges g pairs))))
   (graph/weighted-digraph)
   nodes))

(defn part-1 [input]
  (let [lines (parse-lines input)
        nodes (build-nodes lines)
        g (build-graph nodes)
        start-nodes (filter (fn [node] (= 0 (:v node))) (map first nodes))
        spans (map (fn [start-node] (alg/dijkstra-span g start-node)) start-nodes)
        paths (map (fn [span] (mapcat (fn [[from to]] (filter (fn [[k v]] (= 9 (:v k))) to)) span))  spans)]
    (->> (map count paths)
         (reduce +))))

(defn all-paths
  [g start end-v]
  (letfn [(dfs [current visited path]
            (if (= (:v current) end-v)
              [path]
              (for [neighbor (graph/successors g current)
                    :when (not (visited neighbor))
                    path (dfs neighbor (conj visited neighbor) (conj path neighbor))]
                path)))]
    (dfs start #{start} [start])))

(defn part-2 [input]
  (let [lines (parse-lines input)
        nodes (build-nodes lines)
        g (build-graph nodes)
        start-nodes (filter (fn [node] (= 0 (:v node))) (map first nodes))
        paths (map (fn [node] (all-paths g node 9)) start-nodes)]
    (->> (map count paths)
         (reduce +))))

(comment
  (= 820 (part-1 input))
  (= 1786 (part-2 input)))
