(ns aoc-clj.2015.09.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2015/09/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2015/09/test-input.txt")))

(defn parse-input [line]
  (let [[from to distance] (string/split line #" to | = ")]
    {:from from
     :to to
     :distance (Integer/parseInt distance)}))

(defn distance-graph
  ([instructions] (distance-graph instructions {}))
  ([instructions graph]
   (if-not (seq instructions) graph
           (let [{:keys [from to distance]} (first instructions)
                 from-entry (get graph from [])
                 next-from-entry (conj from-entry {:to to :distance distance})
                 to-entry (get graph to [])
                 next-to-entry (conj to-entry {:to from :distance distance})]
             (recur
              (rest instructions)
              (-> graph (assoc from next-from-entry) (assoc to next-to-entry)))))))

(defn find-paths
  ([graph start] (find-paths graph [start] 0))
  ([graph visited distances]
   (if (= (count graph) (count visited)) distances
       (let [visited-set (apply hash-set visited)
             visit-next (filter (fn [{:keys [to]}] (not (visited-set to))) (graph (last visited)))]
         (map (fn [{:keys [to distance]}] (find-paths graph (conj visited to) (+ distances distance))) visit-next)))))

(defn input->paths [input]
  (let [instructions (map parse-input input)
        graph (distance-graph instructions)
        places (distinct (keys graph))]
    (map (fn [place] (find-paths graph place)) places)))

(defn part-1 [input]
    (apply min (flatten (input->paths input))))

(defn part-2 [input]
    (apply max (flatten (input->paths input))))

(comment
  (= 207 (part-1 input))
  (= 804 (part-2 input)))
