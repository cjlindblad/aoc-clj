(ns aoc-clj.2024.21.solution
  (:require [clojure.string :as str]
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

(defn numpad-path [from to]
  (let [numpad-graph-data (build-graph-data numpad #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \A})
        numpad-graph (graph/weighted-digraph numpad-graph-data)
        with-start (add-start-node numpad-graph from)
        with-goal (add-goal-node with-start to)]
    (->> (alg/dijkstra-path with-goal :start :goal)
         (drop 2)
         (drop-last 1)
         (mapv last))))

(defn numpad-sequence [ns]
  (let [from-tos (partition 2 1 (concat [\A] ns))]
    (->> (map (fn [[from to]] (numpad-path from to)) from-tos)
         (mapv (fn [x] (concat x [\A])))
         (map (partial apply str))
         (apply str))))

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

(defn code-complexity [code]
  (let [seq-1 (numpad-sequence code)
        seq-2 (dirpad-sequence seq-1)
        seq-3 (dirpad-sequence seq-2)
        numeric-part (parse-long (subs code 0 3))]
    (* numeric-part (count seq-3))))

(defn part-1 [input]
  (->> (str/split input #"\n")
       (map code-complexity)
       (reduce +)))

; TODO we need to find ALL shortest paths for each step, and check which is the shortest after all expansions. Solve day 16 first?
(part-1 test-input)
; 185828 too high
; (["029A" 68 29]
;  ["980A" 60 980]
;  ["179A" 68 179]
;  ["456A" 64 456]
;  ["379A" 68 379])

(code-complexity "029A")

(dirpad-sequence (dirpad-sequence (numpad-sequence "379A")))
; "^A^^<<A>>AvvvA"
; "<A>A<AAv<AA>>^AvAA^A<vAAA^>A"
; "v<<A>>^AvA^Av<<A>>^AA<vA<A>>^AAvAA^<A>A<vA^>AA<A>Av<<A>A^>AAA<Av>A^A"

