(ns aoc-clj.2024.18.solution
  (:require [clojure.string :as str]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def input (slurp "src/aoc_clj/2024/18/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/18/test-input.txt"))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (mapv #(read-string (str "[" % "]")))))

(defn create-memory [size]
  (mapv identity (repeat (* size size) \.)))

(defn coord->index [size [x y]]
  (+ x (* y size)))

(defn in-bounds? [size i]
  (<= 0 i (dec (* size size))))

(defn neighbour-indices [i size]
  (filter
   identity
   [(when-not (zero? (mod i size)) (dec i)) ; left
    (when-not (= (dec size) (mod i size)) (inc i)) ; right
    (when-not (zero? (mod (quot i size) size)) (- i size)) ; up
    (when-not (= (dec size) (mod (quot i size) size)) (+ i size)) ; down
    ]))

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

(defn part-2 [input size]
  (let [byte-coords (parse-input input)
        memory (create-memory size)]
    (loop [memory memory coords byte-coords]
      (let [next-memory (place-bytes memory size coords)
            g-data (graph-data next-memory size)
            g (apply graph/weighted-digraph g-data)
            path (alg/dijkstra-path g 0 (dec (* size size)))]
        (if (zero? (count path))
          (recur memory (drop-last coords))
          (let [[x y] (first (drop (count coords) byte-coords))]
            (str x "," y)))))))

(comment
  (= 506 (part-1 input 71 1024))
  (= "62,6" (part-2 input 71)))

