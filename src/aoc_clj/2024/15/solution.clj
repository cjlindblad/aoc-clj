(ns aoc-clj.2024.15.solution
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [loom.graph :as graph]
            [loom.alg :as alg]
            [loom.io :as io]))

(def input (slurp "src/aoc_clj/2024/15/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/15/test-input.txt"))
(def small-test-input (slurp "src/aoc_clj/2024/15/small-test-input.txt"))

(defn parse-map [lines [max-x max-y]]
  (let [positions (->> (for [x (range (inc max-x)) y (range (inc max-y))]
                         [(get-in lines [y x]) [x y]])
                       (group-by first)
                       (map (fn [[k v]] [k (into #{} (map last v))]))
                       (into {}))]
    {:walls (positions \#)
     :boxes (positions \O)
     :robot (positions \@)}))

(defn parse-moves [moves-string]
  (as-> (str/replace moves-string "\n" "") %
    (str/split % #"")
    (map str %)))

(defn parse-input [input]
  (let [[map-string moves-string] (str/split input #"\n\n")
        map-lines (str/split map-string #"\n")
        max-y (dec (count map-lines))
        max-x (dec (count (first map-lines)))]
    {:warehouse (parse-map map-lines [max-x max-y])
     :max-x max-x
     :max-y max-y
     :directions (parse-moves moves-string)}))

(defn at-warehouse-pos [warehouse pos]
  (cond
    ((warehouse :walls) pos) "#"
    ((warehouse :boxes) pos) "O"
    ((warehouse :robot) pos) "@"
    :else "."))

(defn visualize [warehouse [max-x max-y]]
  (->> (for [y (range (inc max-y))]
         (for [x (range (inc max-x))]
           (let [pos [x y]]
             (at-warehouse-pos warehouse pos))))
       (map #(str/join "" %))))

(defn direction-delta [direction]
  (case direction
    "^" [0 -1]
    ">" [1 0]
    "v" [0 1]
    "<" [-1 0]))

(defn apply-delta [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn get-pushable-line [warehouse pos delta]
  (loop [line [] pos pos]
    (let [next-pos (apply-delta pos delta)
          next-thing (at-warehouse-pos warehouse next-pos)]
      (case next-thing
        "#" nil
        "." (conj line next-pos)
        "O" (recur (conj line next-pos) next-pos)))))

(defn move [warehouse direction]
  (let [delta (direction-delta direction)
        robot (first (:robot warehouse))
        next-robot-pos (apply-delta robot delta)
        next-location (at-warehouse-pos warehouse next-robot-pos)]
    (case next-location
      "#" warehouse
      "." (assoc warehouse :robot #{next-robot-pos})
      "O" (let [pushable-line (get-pushable-line warehouse next-robot-pos delta)]
            (if-not pushable-line
              warehouse
              (let [boxes (:boxes warehouse)
                    next-boxes (-> (into boxes pushable-line)
                                   (disj boxes next-robot-pos))]
                (-> (assoc warehouse :robot #{next-robot-pos})
                    (assoc :boxes next-boxes))))))))

(defn score [warehouse]
  (->> (:boxes warehouse)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

(defn part-1 [input]
  (let [{:keys [warehouse directions max-x max-y]} (parse-input input)
        final-warehouse (reduce (fn [w d] (move w d)) warehouse directions)]
    (score final-warehouse)))

(part-1 input) ; 1398947

(comment
  (= 1398947 (part-1 input)))

