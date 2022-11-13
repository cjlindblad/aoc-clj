(ns aoc-clj.2021.05.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2021/05/input.txt")))
(defn parse-line [line]
  (let [[x1 y1 x2 y2] (s/split line #",| -> ")]
    {:x1 (Integer/parseInt x1)
     :y1 (Integer/parseInt y1)
     :x2 (Integer/parseInt x2)
     :y2 (Integer/parseInt y2)}))

(defn horizontal-or-vertical? [{:keys [x1 y1 x2 y2]}]
  (or (= x1 x2)
      (= y1 y2)))

(defn expand-points [{:keys [x1 y1 x2 y2]}]
  (let [min-x (min x1 x2)
        min-y (min y1 y2)
        max-x (max x1 x2)
        max-y (max y1 y2)]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))]
      [x y])))

(defn duplicates [xs]
  (->> xs
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (filter (fn [[_ v]] (< 1 v)))
       (map first)))

(defn part-1 [input]
  (->> input
       (map parse-line)
       (filter horizontal-or-vertical?)
       (mapcat expand-points)
       duplicates
       count))

(defn expand-points-2 [{:keys [x1 y1 x2 y2] :as line}]
  (if (horizontal-or-vertical? line) (expand-points line)
      (let [xs (if (< x1 x2) (range x1 (inc x2)) (range x1 (dec x2) -1))
            ys (if (< y1 y2) (range y1 (inc y2)) (range y1 (dec y2) -1))]
        (map vector xs ys))))

(defn part-2 [input]
  (->> input
       (map parse-line)
       (mapcat expand-points-2)
       duplicates
       count))

(comment
  (= 6397 (part-1 input))
  (= 22335 (part-2 input)))
