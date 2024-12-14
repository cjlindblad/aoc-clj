(ns aoc-clj.2024.14.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/14/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/14/test-input.txt"))

(defn parse-long [n] (Long/parseLong n))

(defn parse-line [line]
  (let [[x y dx dy]
        (->> (re-seq #"p\=(.+),(.+) v\=(.+),(.+)" line)
             first
             (drop 1)
             (map parse-long))]
    [[x y] [dx dy]]))

(defn parse-input [input]
  (->> (str/split input #"\n")
       (map parse-line)))

(defn map-size [input]
  (let [[xs ys]
        (->> (map first input)
             (reduce (fn [[xs ys] [x y]] [(conj xs x) (conj ys y)]) [[] []]))]
    [(apply max xs)
     (apply max ys)]))

(defn move [[[x y] [dx dy]] [max-x max-y]]
  [[(mod (+ x dx) (inc max-x))
    (mod (+ y dy) (inc max-y))]
   [dx dy]])

(defn simulate-step [robots [max-x max-y]]
  (map (fn [robot] (move robot [max-x max-y])) robots))

(defn simulate-steps [robots [max-x max-y] n]
  (loop [robots robots n n]
    (if (zero? n)
      robots
      (recur (simulate-step robots [max-x max-y]) (dec n)))))

(defn quadrant [[x y] [max-x max-y]]
  (let [middle-x (/ max-x 2)
        middle-y (/ max-y 2)]
    (if (or (= x middle-x) (= y middle-y))
      nil
      (cond
        (and (< x middle-x) (< y middle-y)) :NW
        (and (< x middle-x) (> y middle-y)) :SW
        (and (> x middle-x) (< y middle-y)) :NE
        (and (> x middle-x) (> y middle-y)) :SE))))

(defn part-1 [input]
  (let [robots (parse-input input)
        [max-x max-y] (map-size robots)
        moved-robots (simulate-steps robots [max-x max-y] 100)
        robots-by-quadrant (group-by (fn [[[x y] _]] (quadrant [x y] [max-x max-y])) moved-robots)]
    (->> (filter (fn [[k _]] k) robots-by-quadrant)
         (map (fn [[_ v]] (count v)))
         (reduce *))))

(defn simulate-until-unique [robots [max-x max-y]]
  (let [robot-count (count robots)]
    (loop [robots robots n 0]
      (let [coord-set (into #{} (map (fn [[coord _]] coord) robots))
            set-count (count coord-set)]
        (if (= set-count robot-count)
          n
          (recur (simulate-step robots [max-x max-y]) (inc n)))))))

(defn part-2 [input]
  (let [robots (parse-input input)
        [max-x max-y] (map-size robots)]
    (simulate-until-unique robots [max-x max-y])))

(comment
  (= 222208000 (part-1 input))
  (= 7623 (part-2 input)))
