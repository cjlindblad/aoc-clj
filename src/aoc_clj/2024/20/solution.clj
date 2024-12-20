(ns aoc-clj.2024.20.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/20/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/20/test-input.txt"))

(defn valid-coord? [lines [x y]]
  (#{\. \S \E} (get-in lines [y x])))

(defn build-graph-data [input]
  (let [lines (str/split input #"\n")
        start-coord (atom nil)
        end-coord (atom nil)
        skips (atom [])]
    (as->
     (for [y (range (count lines))
           x (range (count (first lines)))]
       (let [current-char (get-in lines [y x])]
         (when (= \S current-char)
           (reset! start-coord [x y]))
         (when (= \E current-char)
           (reset! end-coord [x y]))
         (when (and (valid-coord? lines [x y])
                    (not (valid-coord? lines [(inc x) y]))
                    (valid-coord? lines [(+ 2 x) y]))
           (swap! skips conj [[x y] [(inc x) y] [(+ 2 x) y]]))
         (when (and (valid-coord? lines [x y])
                    (not (valid-coord? lines [x (inc y)]))
                    (valid-coord? lines [x (+ 2 y)]))
           (swap! skips conj [[x y] [x (inc y)] [x (+ 2 y)]]))
         (let [neighbour-coords (map (partial mapv + [x y]) [[0 1] [1 0] [-1 0] [0 -1]])
               valid-neighbours (filter (partial valid-coord? lines) neighbour-coords)]
           [[x y] (into {} (vec (mapv (fn [v] [v 1]) valid-neighbours)))])))
     %
      (into {} %)
      (assoc % :start {@start-coord 0})
      (assoc % @end-coord {:end 0})
      {:data %
       :skips @skips})))

(def smol-map (str/join "\n" ["S..#E"
                              "##.#."
                              "...#."
                              ".#.#."
                              ".###."
                              "....."]))

(defn part-1 [input limit]
  (let [{:keys [data skips]} (build-graph-data input)
        g (graph/weighted-digraph data)
        [path distance] (alg/dijkstra-path-dist g :start :end)
        cheat-paths (for [[skip-from skip skip-to] skips]
                      (let [skip? (atom false)]
                        (->> (reduce (fn [result coord]
                                       (let [next-result (if @skip? result (conj result coord))]
                                         (when (#{skip-from skip-to} coord)
                                           (swap! skip? not))
                                         next-result))
                                     []
                                     path))))
        cheat-lengths (map (partial + 2) (map count cheat-paths))]
    (->> (map (fn [cl] (- (count path) cl)) cheat-lengths)
         (filter (partial <= limit))
         count)))

(comment
  (= 1393 (part-1 input 100)))
