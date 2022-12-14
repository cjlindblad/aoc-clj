(ns aoc-clj.2022.14.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/14/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/14/test-input.txt")))

(defn parse-coords [line]
  (->> (string/split line #" -> ")
       (map #(string/split % #","))
       (map (partial map #(Integer/parseInt %)))))

(defn expand-path
  ([coords] (expand-path coords []))
  ([coords result]
   (if (seq (rest coords))
     (let [[[x1 y1] [x2 y2]] coords
           path (if (= x1 x2)
                  (for [y (range (min y1 y2) (inc (max y1 y2)))] [x1 y])
                  (for [x (range (min x1 x2) (inc (max x1 x2)))] [x y1]))]
       (recur (rest coords) (concat result path)))
     result)))

(defn next-sand-coord [occupied [x y]]
  (let [down [x (inc y)]
        down-left [(dec x) (inc y)]
        down-right [(inc x) (inc y)]]
    (cond ((complement occupied) down) down
          ((complement occupied) down-left) down-left
          ((complement occupied) down-right) down-right
          :else :resting)))

(defn drop-sand
  ([occupied lowest-y] (drop-sand occupied lowest-y [500 0]))
  ([occupied lowest-y current-coord]
   (let [next-coord (next-sand-coord occupied current-coord)]
     (cond (= :resting next-coord) current-coord
           (= lowest-y (last next-coord)) :abyss
           :else (recur occupied lowest-y next-coord)))))

(defn drop-sand-2
  ([occupied lowest-y] (drop-sand-2 occupied lowest-y [500 0]))
  ([occupied lowest-y current-coord]
   (let [next-coord (next-sand-coord occupied current-coord)]
     (cond (= :resting next-coord) current-coord
           (= (inc lowest-y) (last next-coord)) next-coord
           :else (recur occupied lowest-y next-coord)))))

(defn solver
  ([rocks lowest-y] (solver lowest-y (set rocks) 0))
  ([lowest-y occupied units]
   (let [resting-coord (drop-sand occupied lowest-y)]
     (if (= :abyss resting-coord) units
         (recur lowest-y (conj occupied resting-coord) (inc units))))))

(defn solver-2
  ([rocks lowest-y] (solver-2 lowest-y (set rocks) 0))
  ([lowest-y occupied units]
   (let [resting-coord (drop-sand-2 occupied lowest-y)]
     (if (= [500 0] resting-coord) units
         (recur lowest-y (conj occupied resting-coord) (inc units))))))

(defn part-1 [input]
  (let [coords (map parse-coords input)
        rocks (set (mapcat expand-path coords))
        lowest-y (apply max (map second rocks))
        result (solver rocks lowest-y)]
    result))

(defn part-2 [input]
  (let [coords (map parse-coords input)
        rocks (set (mapcat expand-path coords))
        lowest-y (apply max (map second rocks))
        result (solver-2 rocks lowest-y)]
    (inc result)))

(comment
  (= 885 (part-1 input))
  (= 28691 (part-2 input)))
