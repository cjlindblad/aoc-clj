(ns aoc-clj.2022.12.solution
  (:require [clojure.string :as string]
            [loom.graph :as graph]
            [loom.alg :as alg]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/12/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/12/test-input.txt")))

(defn parse-input [input]
  (let [coords (for [y (range 0 (count input)) x (range 0 (count (first input)))] [x y])
        chars (apply str input)]
    {:coord-to-char (zipmap coords chars)
     :char-to-coord (zipmap chars coords)}))

(defn get-neighbour-coords [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn elevation [c]
  (case c
    \S \a
    \E \z
    c))

(defn valid-step? [from to]
  (>= 1 (- (int to) (int from))))

(defn valid-neighbour? [heightmap current-elevation neighbour-coord]
  (let [neighbour-elevation (elevation (heightmap neighbour-coord))]
    (if neighbour-elevation (valid-step? current-elevation neighbour-elevation)
        false)))

(defn get-valid-neighbours [heightmap coord neighbour-coords]
  (let [current-elevation (elevation (heightmap coord))
        valid-neighbours (filter (partial valid-neighbour? heightmap current-elevation) neighbour-coords)]
    valid-neighbours))

(defn build-coord-graph
  ([heightmap] (build-coord-graph heightmap heightmap {}))
  ([heightmap remaining graph]
   (if (= (count heightmap) (count graph)) (graph/digraph graph)
       (let [[current-coord] (first remaining)
             neighbours (get-valid-neighbours heightmap current-coord (get-neighbour-coords current-coord))]
         (recur heightmap (rest remaining) (assoc graph current-coord neighbours))))))

(defn part-1 [input]
  (let [{:keys [coord-to-char char-to-coord]} (parse-input input)
        graph (build-coord-graph coord-to-char)
        start-coord (char-to-coord \S)
        end-coord (char-to-coord \E)
        result (alg/bf-path graph start-coord end-coord)]
    (dec (count result))))

(defn part-2 [input]
  (let [{:keys [coord-to-char char-to-coord]} (parse-input input)
        graph (build-coord-graph coord-to-char)
        start-coords (map first (filter (fn [[_ char]] (= char \a)) coord-to-char))
        end-coord (char-to-coord \E)
        results (map (fn [start-coord] (alg/bf-path graph start-coord end-coord)) start-coords)
        counts (map count results)]
    (dec (apply min (filter pos? counts)))))

(comment
  (= 497 (part-1 input))
  (= 492 (part-2 input)))

