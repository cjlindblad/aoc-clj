(ns aoc-clj.2024.16.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/16/input.txt"))
(def small-test-input (slurp "src/aoc_clj/2024/16/small-test-input.txt"))
(def large-test-input (slurp "src/aoc_clj/2024/16/large-test-input.txt"))

(defn coords->direction [from to]
  (let [[dx dy] (mapv - to from)]
    (case [dx dy]
      [0 1] :s
      [0 -1] :n
      [1 0] :e
      [-1 0] :w)))

(defn build-graph-data [input]
  (let [start-node (atom nil)
        end-coord (atom nil)
        graph-data
        (as->
         (let [lines (str/split input #"\n")]
           (mapcat
            identity
            (for [y (range (count lines))
                  x (range (count (first lines)))]
              (let [current-char (get-in lines [y x])
                    neighbours (mapv (partial mapv + [x y]) [[1 0] [0 1] [-1 0] [0 -1]])
                    valid-neighbours (vec (filter (fn [[x y]] (#{\. \S \E} (get-in lines [y x]))) neighbours))]
                (when (= \S current-char)
                  (reset! start-node [x y :e]))
                (when (= \E current-char)
                  (reset! end-coord [x y]))
                (for [direction [:n :e :s :w]]
                  [[x y direction]
                   (into
                    {}
                    (mapv
                     (fn [[n-x n-y]]
                       (let [n-direction (coords->direction [x y] [n-x n-y])]
                         [[n-x n-y n-direction] (if (= n-direction direction) 1 1001)]))
                     valid-neighbours))])))))
         %
          (into {} %)
          (assoc % :start {@start-node 0})
          (assoc % :goal {(conj @end-coord :n) 0
                          (conj @end-coord :e) 0
                          (conj @end-coord :s) 0
                          (conj @end-coord :w) 0})
          (assoc % (conj @end-coord :n) (assoc (% (conj @end-coord :n)) :goal 0))
          (assoc % (conj @end-coord :e) (assoc (% (conj @end-coord :e)) :goal 0))
          (assoc % (conj @end-coord :s) (assoc (% (conj @end-coord :s)) :goal 0))
          (assoc % (conj @end-coord :w) (assoc (% (conj @end-coord :w)) :goal 0)))]
    graph-data))

(defn part-1 [input]
  (let [graph-data (build-graph-data input)
        g (graph/weighted-digraph graph-data)
        [_ distance] (alg/dijkstra-path-dist g :start :goal)]
    distance))

(comment
  (= 106512 (time (part-1 input))))
