(ns aoc-clj.2024.08.solution
  (:require [clojure.string :as str]))

(def input (str/split (slurp "src/aoc_clj/2024/08/input.txt") #"\n"))
(def test-input (str/split (slurp "src/aoc_clj/2024/08/test-input.txt") #"\n"))

(defn antenna-positions [input]
  (for [y (range (count input))
        x (range (count (first input)))
        :when (not= (get-in input [y x]) \.)]
    [(get-in input [y x]) [x y]]))

(defn positions-by-antenna [input]
  (->> (antenna-positions input)
       (group-by first)
       (map (fn [[antenna positions]] [antenna (mapv last positions)]))
       (into {})))

(defn all-pairs [things]
  (for [i things j things :when (not= i j)]
    [i j]))

(defn inside? [[max-x max-y] [x y]]
  (and (<= 0 x max-x) (<= 0 y max-y)))

(defn antinode-position [[[x1 y1] [x2 y2]]]
  (let [xd (- x1 x2) yd (- y1 y2)]
    [(+ x1 xd) (+ y1 yd)]))

; TODO does not care if they are inside or not
(defn positions->antinodes [positions]
  (->> (map all-pairs positions)
       (mapcat (partial map antinode-position))
       distinct))

(defn antinode-position-2 [[max-x max-y] [[x1 y1] [x2 y2]]]
  (let [dx (- x1 x2) dy (- y1 y2)]
    (loop [antinodes [] [x y] [x1 y1]]
      (let [next-pos [(+ x dx) (+ y dy)]]
        (if (inside? [max-x max-y] next-pos)
          (recur (conj antinodes next-pos) next-pos)
          antinodes)))))

(defn positions->antinodes-2 [[max-x max-y] positions]
  (->> (map all-pairs positions)
       (mapcat (partial mapcat (partial antinode-position-2 [max-x max-y])))
       distinct))

(defn part-1 [input]
  (let [max-y (dec (count input))
        max-x (dec (count (first input)))
        positions (map last (positions-by-antenna input))
        antinodes (filter (partial inside? [max-x max-y]) (positions->antinodes positions))]
    (count antinodes)))

(defn part-2 [input]
  (let [max-y (dec (count input))
        max-x (dec (count (first input)))
        positions (filter #(< 1 (count %)) (map last (positions-by-antenna input)))
        antinodes (positions->antinodes-2 [max-x max-y] positions)]
    (count (distinct (concat antinodes (mapcat identity positions))))))

(comment
  (= 367 (part-1 input))
  (= 1285 (part-2 input)))

