(ns aoc-clj.2024.12.solution
  (:require [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def input (slurp "src/aoc_clj/2024/12/input.txt"))

(defn parse-lines [input]
  (mapv #(str/split % #"") (str/split input #"\n")))

(def deltas [[0 1] [1 0] [0 -1] [-1 0]])

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
        coord->neighbours (map (fn [coord] [coord (neighbour-coords coord max-x max-y)]) coords)]
    (map (fn [[coord neighbours]]
           (let [coord-node (get-node coord)]
             [coord-node (mapv identity (filter (fn [{:keys [v]}] (= (:v coord-node) v)) (mapv get-node neighbours)))]))
         coord->neighbours)))

(defn build-graph [nodes]
  (reduce (fn [g [node neighbours]]
            (if (empty? neighbours)
              (graph/add-edges g [node node])
              (let [pairs (for [neighbour neighbours] [node neighbour])]
                (apply graph/add-edges g pairs))))
          (graph/digraph)
          nodes))

(defn fence-edges [lines [x y]]
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

(def direction-deltas
  [[[0 1] :S]
   [[1 0] :E]
   [[0 -1] :N]
   [[-1 0] :W]])

(defn boundless-neighbour-direction-coords [[x y]]
  (->> (map (fn [[[dx dy] direction]] [[(+ x dx) (+ y dy)] direction]) direction-deltas)))

(defn fence-edges-2 [lines [x y]]
  (let [value (get-in lines [y x])
        neighbours (boundless-neighbour-direction-coords [x y])
        neighbour-vals (map (fn [[[x y] direction]] [direction (get-in lines [y x])]) neighbours)]
    (->> (filter (fn [[_ v]] (not= v value)) neighbour-vals)
         (map (fn [[direction]] [[x y] direction])))))

(defn partition-between [pred? coll]
  (let [switch (reductions not= true (map pred? coll (rest coll)))]
    (map (partial map first) (partition-by second (map list coll switch)))))

(def partition-increasing-by-one
  (partial partition-between (fn [a b] (not= 1 (- b a)))))

(defn line-partitions [f g edges]
  (->> edges
       (group-by g)
       (map (fn [[_ to-be-partitioned]] (map f to-be-partitioned)))
       (map sort)
       (mapcat partition-increasing-by-one)
       count))

(defn count-fence-lines [edges]
  (->> (group-by last edges)
       (map (fn [[k v]] [k (sort (map first v))]))
       (map (fn [[direction edges]]
              (->> (case direction
                     :N (line-partitions first last edges)
                     :S (line-partitions first last edges)
                     :W (line-partitions last first edges)
                     :E (line-partitions last first edges)))))
       (reduce +)))

(defn part-2 [input]
  (let [lines (parse-lines input)
        nodes (build-nodes lines)
        graph (build-graph nodes)
        regions (map (partial map (juxt :x :y)) (alg/scc graph))
        edges (map (partial mapcat (fn [[x y]] (fence-edges-2 lines [x y]))) regions)]
    (->> (map count-fence-lines edges)
         (map (fn [region sides] (* (count region) sides)) regions)
         (reduce +))))

(comment
  (= 1421958 (part-1 input))
  (= 885394 (part-2 input)))
