(ns aoc-clj.2022.17.solution
  (:require [clojure.string :as string]))

(def input (string/trim (slurp "src/aoc_clj/2022/17/input.txt")))
(def test-input (string/trim (slurp "src/aoc_clj/2022/17/test-input.txt")))

(def piece-types [:- :+ :angle :| :square])
(def left-edge 2)

(defn make-piece [type highest-y]
  (let [bottom-edge (+ highest-y 4)]
    (case type
      :-
      (let [xs (range left-edge (+ left-edge 4))
            ys (repeat bottom-edge)]
        (map vector xs ys))
      :+
      [[left-edge (inc bottom-edge)]
       [(inc left-edge) (inc bottom-edge)]
       [(+ 2 left-edge) (inc bottom-edge)]
       [(inc left-edge) (+ 2 bottom-edge)]
       [(inc left-edge) bottom-edge]]
      :angle
      [[left-edge bottom-edge]
       [(inc left-edge) bottom-edge]
       [(+ 2 left-edge) bottom-edge]
       [(+ 2 left-edge) (inc bottom-edge)]
       [(+ 2 left-edge) (+ 2 bottom-edge)]]
      :|
      [[left-edge bottom-edge]
       [left-edge (inc bottom-edge)]
       [left-edge (+ 2 bottom-edge)]
       [left-edge (+ 3 bottom-edge)]]
      :square
      [[left-edge bottom-edge]
       [(inc left-edge) bottom-edge]
       [left-edge (inc bottom-edge)]
       [(inc left-edge) (inc bottom-edge)]])))

(defn move-piece [direction existing piece]
  (let [next-piece (case direction
                     :down (map (fn [[x y]] [x (dec y)]) piece)
                     \< (map (fn [[x y]] [(dec x) y]) piece)
                     \> (map (fn [[x y]] [(inc x) y]) piece))
        illegal (or (some existing next-piece)
                    (some (fn [[x y]] (or (neg? y) (neg? x) (<= 7 x))) next-piece))]
    (if illegal piece next-piece)))

(defn highest-edge [piece]
  (->> (map last piece)
       (apply max)))

(defn rotate [xs]
  (conj (into [] (rest xs)) (first xs)))

(defn print-step [existing piece]
  (let [strings (->> (for [y (range (highest-edge piece) -1 -1) x (range 0 7)]
                       [x y])
                     (partition 7)
                     (map (partial map #(if (existing %) \# (if ((set piece) %) \@ \.))))
                     (map (partial apply str)))]
    (doall (map println strings))))

(defn play-tetris
  ([piece-types instructions limit] (play-tetris piece-types instructions nil -1 #{} limit 0))
  ([piece-types instructions current-piece highest-y existing limit round]
   (println round)
   (if (= round limit) (inc highest-y)
     (if-not current-piece
       (let [current-type (first piece-types)
             piece (make-piece current-type highest-y)]
         (recur (rotate piece-types) instructions piece highest-y existing limit round))
       (let [direction (first instructions)
             jet-moved-piece (move-piece direction existing current-piece)
             down-moved-piece (move-piece :down existing jet-moved-piece)]
         (if (not= down-moved-piece jet-moved-piece)
           (recur piece-types (rotate instructions) down-moved-piece highest-y existing limit round)
           (recur piece-types (rotate instructions) nil (max highest-y (highest-edge down-moved-piece)) (into existing down-moved-piece) limit (inc round))))))))

(defn part-1 [input]
  (play-tetris piece-types input 2022)
  )

(part-1 input)
;; => 3200

(comment
  (= 3200 (part-1 input))
  )

(range 10 0 -1)

;; => 2891 TOO LOW
