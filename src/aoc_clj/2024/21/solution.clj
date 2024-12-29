(ns aoc-clj.2024.21.solution
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/21/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/21/test-input.txt"))

(defn parse-long [n] (Long/parseLong n))

(def numpad ["789"
             "456"
             "123"
             " 0A"])

(def dirpad [" ^A"
             "<v>"])

(defn coords->direction [from to]
  (let [[dx dy] (mapv - to from)]
    (case [dx dy]
      [0 1] \v
      [0 -1] \^
      [1 0] \>
      [-1 0] \<)))

(defn build-graph-data [lines valid-chars]
  (let [start-coord (atom nil)
        graph-data
        (as->
         (mapcat
          identity
          (for [y (range (count lines))
                x (range (count (first lines)))
                :when (valid-chars (get-in lines [y x]))]
            (let [current-char (get-in lines [y x])
                  neighbours (mapv (partial mapv + [x y]) [[1 0] [0 1] [-1 0] [0 -1]])
                  valid-neighbours (vec (filter (fn [[x y]] (valid-chars (get-in lines [y x]))) neighbours))]
              (when (= \A current-char)
                (reset! start-coord [current-char]))
              (for [direction [\^ \> \v \<]]
                [[current-char direction]
                 (into
                  {}
                  (mapv
                   (fn [[n-x n-y]]
                     (let [n-direction (coords->direction [x y] [n-x n-y])
                           next-char (get-in lines [n-y n-x])]
                       [[next-char n-direction] (if (= n-direction direction) 1 1001)]))
                   valid-neighbours))]))))
         %
          (into {} %)
          #_(assoc % :start {(conj @start-coord \^) 0
                             (conj @start-coord \>) 0
                             (conj @start-coord \v) 0
                             (conj @start-coord \<) 0}))]
    graph-data))

(defn add-start-node [g start-char]
  (let [start-nodes (filter
                     (fn [node]
                       (= start-char (first node)))
                     (graph/nodes g))
        nodes-to-add (mapv (fn [start-node] [:start start-node 0]) start-nodes)]
    (apply graph/add-edges g nodes-to-add)))

(defn add-goal-node [g goal-char]
  (let [end-nodes (filter
                   (fn [node]
                     (and (not= :start node)
                          (= goal-char (first node))))
                   (graph/nodes g))
        nodes-to-add (mapv (fn [end-node] [end-node :goal 0]) end-nodes)]
    (apply graph/add-edges g nodes-to-add)))

(defn valid-path? [area [start-x start-y] ds]
  (let [path (loop [result [(get-in area [start-y start-x])]
                    x start-x
                    y start-y
                    ds ds]
               (if (seq ds)
                 (let [[next-x next-y] (mapv + [x y] (first ds))]
                   (recur (conj result (get-in area [next-y next-x]))
                          next-x
                          next-y
                          (rest ds)))
                 result))]
    (not-any? #{\space} path)))

(defn delta->direction [[dx dy]]
  (case [dx dy]
    [1 0] \>
    [-1 0] \<
    [0 1] \v
    [0 -1] \^))

(defn paths [area from to]
  ; TODO a bit slow, but hey..
  (let [from-coord (first (for [y (range (count area))
                                x (range (count (first area)))
                                :when (= from (get-in area [y x]))]
                            [x y]))
        to-coord (first (for [y (range (count area))
                              x (range (count (first area)))
                              :when (= to (get-in area [y x]))]
                          [x y]))
        [dx dy] (mapv - to-coord from-coord)
        dxs (repeat (Math/abs dx) (if (pos? dx) [1 0] [-1 0]))
        dys (repeat (Math/abs dy) (if (pos? dy) [0 1] [0 -1]))
        ds (filter (fn [permutation] (valid-path? area from-coord permutation)) (combo/permutations (concat dxs dys)))]
    (mapv (partial mapv delta->direction) ds)))

(defn all-paths [area cs]
  (let [from-tos (partition 2 1 (concat [\A] cs))
        ps (mapv (fn [[from to]] (paths area from to)) from-tos)]
    (->> (apply combo/cartesian-product ps)
         (map (partial map (fn [x] (conj x \A))))
         (map (partial reduce concat))
         (map (partial apply str)))))

;; TODO could be parameterized and merged with numpad-path
(defn dirpad-path [from to]
  (let [dirpad-graph-data (build-graph-data dirpad #{\< \^ \> \v \A})
        dirpad-graph (graph/weighted-digraph dirpad-graph-data)
        with-start (add-start-node dirpad-graph from)
        with-goal (add-goal-node with-start to)]
    (->> (alg/dijkstra-path with-goal :start :goal)
         (drop 2)
         (drop-last 1)
         (mapv last))))

(defn dirpad-sequence [ds]
  (let [from-tos (partition 2 1 (concat [\A] ds))]
    (->> (map (fn [[from to]] (dirpad-path from to)) from-tos)
         (mapv (fn [x] (concat x [\A])))
         (map (partial apply str))
         (apply str))))

(defn code-complexity-new [code]
  (let [num-paths (all-paths numpad code)
        dir-paths (map dirpad-sequence num-paths)
        dir-paths-2 (map dirpad-sequence dir-paths)
        numeric-part (parse-long (subs code 0 3))]
    (apply min (map #(* numeric-part (count %)) dir-paths-2))))

(defn part-1 [input]
  (->> (str/split input #"\n")
       (map code-complexity-new)
       (reduce +)))

(comment
  (= 179444 (part-1 input)))

