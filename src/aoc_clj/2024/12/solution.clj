(ns aoc-clj.2024.12.solution
  (:require [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def input (slurp "src/aoc_clj/2024/12/input.txt"))
(def small-test-input (slurp "src/aoc_clj/2024/12/small-test-input.txt"))
(def larger-test-input (slurp "src/aoc_clj/2024/12/larger-test-input.txt"))

(defn parse-lines [input]
  (mapv #(str/split % #"") (str/split input #"\n")))

(def deltas [[0 1]
             [1 0]
             [0 -1]
             [-1 0]])

(defn neighbour-coords [[x y] max-x max-y]
  (->> (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)
       (filter (fn [[x y]] (and (<= 0 x max-x) (<= 0 y max-y))))))

(defn boundless-neighbour-coords [[x y]]
  (->> (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) deltas)))

(defn build-nodes [lines]
  (let [get-node (fn [[x y]] {:x x :y y :v (get-in lines [y x])})
        max-x (dec (count (first lines)))
        max-y (dec (count lines))
        coords (for [x (range (inc max-x)) y (range (inc max-y))] [x y])
        coord->neighbours (map (fn [coord] [coord (neighbour-coords coord max-x max-y)]) coords)
        nodes (map
               (fn [[coord neighbours]]
                 (let [coord-node (get-node coord)]
                   [coord-node (mapv identity (filter (fn [{:keys [v]}] (= (:v coord-node) v)) (mapv get-node neighbours)))]))
               coord->neighbours)]
    nodes))

(defn build-graph [nodes]
  (reduce
   (fn [g [node neighbours]]
     (if (empty? neighbours)
       (graph/add-edges g [node node])
       (let [pairs (for [neighbour neighbours] [node neighbour])]
         (apply graph/add-edges g pairs))))
   (graph/digraph)
   nodes))

(defn fence-edges [lines [x y]]
  ; TODO repetition of max-x/max-y
  (let [value (get-in lines [y x])
        neighbours (boundless-neighbour-coords [x y])
        neighbour-vals (map (fn [[x y]] (get-in lines [y x])) neighbours)]
    (-> (filter (complement #{value}) neighbour-vals)
        count)))

(defn part-1 [input]
  (let [lines (parse-lines input)
        nodes (build-nodes lines)
        graph (build-graph nodes)
        regions (map (partial map (juxt :x :y)) (alg/scc graph))
        edges (map (partial map (fn [[x y]] (fence-edges lines [x y]))) regions)
        prices (map (fn [es] (* (count es) (reduce + es))) edges)]
    (reduce + prices)))

(comment
  (= 1421958 (part-1 input)))
