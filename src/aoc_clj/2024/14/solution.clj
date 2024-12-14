(ns aoc-clj.2024.14.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

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
  (loop [robots robots n 100]
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
    (->> (filter (fn [[k v]] k) robots-by-quadrant)
         (map (fn [[k v]] (count v)))
         (reduce *))))

(comment
  (= 222208000 (part-1 input)))
