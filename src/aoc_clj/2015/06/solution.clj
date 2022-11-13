(ns aoc-clj.2015.06.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (s/split-lines (slurp "src/aoc_clj/2015/06/input.txt")))
(defn parse-coords [line]
  (->> (s/split line #"\D+")
       (filter (complement s/blank?))
       (map parse-int)
       (partition 2)))

(defn parse-mode [line]
  (cond (re-seq #"turn on" line) :turn-on
        (re-seq #"turn off" line) :turn-off
        (re-seq #"toggle" line) :toggle))

(defn expand-coords [[[x1 y1] [x2 y2]]]
  (let [xs (range (min x1 x2) (inc (max x1 x2)))
        ys (range (min y1 y2) (inc (max y1 y2)))]
    (for [x xs y ys]
      [x y])))

(defn parse-instruction [line]
  {:mode (parse-mode line)
   :coords (expand-coords (parse-coords line))})

(defn turn-on [grid coord]
  (conj grid coord))

(defn turn-off [grid coord]
  (disj grid coord))

(defn toggle [grid coord]
  (if (grid coord) (turn-off grid coord)
      (turn-on grid coord)))

(defn handle-lights [grid {:keys [mode coords]}]
  (case mode
    :turn-on (reduce turn-on grid coords)
    :turn-off (reduce turn-off grid coords)
    :toggle (reduce toggle grid coords)))

(defn part-1 [input]
  (->> input
       (map parse-instruction)
       (reduce handle-lights #{})
       count))

(defn turn-on-2 [grid coord]
  (let [existing-coord (grid coord)]
    (if existing-coord (assoc grid coord (inc existing-coord))
        (assoc grid coord 1))))

(defn turn-off-2 [grid coord]
  (let [existing-coord (grid coord)]
    (if (and existing-coord (< 0 existing-coord)) (assoc grid coord (dec existing-coord))
        (assoc grid coord 0))))

(defn toggle-2 [grid coord]
  (let [existing-coord (grid coord)]
    (if existing-coord (assoc grid coord (+ 2 existing-coord))
        (assoc grid coord 2))))

(defn handle-lights-2 [grid {:keys [mode coords]}]
  (case mode
    :turn-on (reduce turn-on-2 grid coords)
    :turn-off (reduce turn-off-2 grid coords)
    :toggle (reduce toggle-2 grid coords)))

(defn part-2 [input]
  (->> input
       (map parse-instruction)
       (reduce handle-lights-2 {})
       vals
       (reduce +)))

(comment
  (= 377891 (part-1 input))
  (= 14110788 (part-2 input)))
