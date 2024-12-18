(ns aoc-clj.2024.18.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/18/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/18/test-input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map #(read-string (str "[" % "]")))))

(defn create-memory [size]
  (mapv identity (repeat (* size size) \.)))

(defn coord->index [size [x y]]
  (+ x (* y size)))

(defn in-bounds? [size i]
  (<= 0 i (dec (* size size))))

; TODO do not skip from right side to left
(defn neighbour-indices [coord size]
  (->> (mapv (partial mapv + coord) [[1 0] [0 1] [-1 0] [0 -1]])
       (map (partial coord->index size))
       (filter (partial in-bounds? size))))

(defn neighbour-indices [i size]
  (filter identity [(when-not (zero? (mod i size)) (dec i))
                    (when-not (zero? (mod i (dec size))) (inc i))
                    (when-not (zero? (mod (quot i size) size)) (- i size))
                    (when-not (= (dec size) (mod (quot i size) size)) (+ i size))])

  #_(->> (mapv (partial + i) [1 -1 size (- size)])
         (filter (partial in-bounds? size))))

(neighbour-indices 6 7)

(defn graph-data [memory  size]
  (->> (for [i (range (* size size))]
         (let [neighbours (->> (map (fn [neighbour-i] [neighbour-i (memory neighbour-i)]) (neighbour-indices i size))
                               (filter (fn [[_ c]] (= \. c)))
                               (map (fn [[i _]] i)))]
           (for [neighbour neighbours]
             [i neighbour])))
       (mapcat identity)))

(defn print-memory [memory size]
  (->> (partition size memory)
       (map str/join)
       (str/join "\n")
       println))

(defn place-bytes [memory size byte-coords]
  (reduce (fn [memory coord] (assoc memory (coord->index size coord) \#)) memory byte-coords))

(defn part-1 [input size byte-count]
  (let [byte-coords (take byte-count (parse-input input))
        memory (create-memory size)
        memory-with-bytes (place-bytes memory size byte-coords)
        g-data (graph-data memory-with-bytes size)
        g (apply graph/digraph g-data)]
    (->> (alg/bf-path g 0 (dec (* size size)))
         count
         dec)))

(comment
  (= 506 (part-1 input 71 1024)))

