(ns aoc-clj.2018.03.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2018/03/input.txt")))

(defn parse-claim [line]
  (let [[_ id left top width height] (first (re-seq #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" line))]
    {:id (Integer/parseInt id)
     :left (Integer/parseInt left)
     :top (Integer/parseInt top)
     :width (Integer/parseInt width)
     :height (Integer/parseInt height)}))

(defn claim->coordinates [{:keys [id left top width height]}]
  {:id id
   :coordinates (for [x (range (inc left) (inc (+ left width)))
                      y (range (inc top) (inc (+ top height)))]
                  [x y])})

(defn overlapping-claims [input]
  (->> input
       (map parse-claim)
       (map claim->coordinates)
       (map :coordinates)
       (apply concat)
       (group-by identity)
       vals
       (filter #(> (count %) 1))))

(defn part-1 [input]
  (->> (overlapping-claims input)
       count))

(defn overlapping-claims-set [input]
  (->> (overlapping-claims input)
       (apply concat)
       (apply hash-set)))

(defn part-2 [input]
  (let [overlapping-set (overlapping-claims-set input)
        claim-coordinates (map claim->coordinates (map parse-claim input))]
    (->> (filter
          (fn [claim] (let [coord-set (apply hash-set (:coordinates claim))]
                        (= coord-set (set/difference coord-set overlapping-set))))
          claim-coordinates)
         first
         :id)))

(comment
  (= 104241 (part-1 input))
  (= 806 (part-2 input)))
