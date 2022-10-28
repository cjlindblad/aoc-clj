(ns aoc-clj.2016.01.solution
  (:require [clojure.string :as s]
            [aoc-clj.utils :as u]))

(def input
  (s/split
   (s/trim
    (slurp "src/aoc_clj/2016/01/input.txt")) #", "))

(defn parse-instruction [instruction]
  (let [turn (first instruction)
        distance (Integer/parseInt (apply str (rest instruction)))]
    [(keyword (str turn)) distance]))

(def directions
  {[:N :R] :E
   [:N :L] :W
   [:E :R] :S
   [:E :L] :N
   [:S :R] :W
   [:S :L] :E
   [:W :R] :N
   [:W :L] :S})

(defn expand-instruction [direction distance]
  (repeat distance [direction 1]))

(defn walk-instruction [visited [turn distance]]
  (let [[prev-direction _] (last visited)
        next-direction (get directions [prev-direction turn])
        steps (expand-instruction next-direction distance)]
    (concat visited steps)))

(defn step-to-coordinate [coordinates [direction _]]
  (let [[prev-x prev-y] (last coordinates)
        next-coordinate (case direction
                          :N [prev-x (inc prev-y)]
                          :E [(inc prev-x) prev-y]
                          :S [prev-x (dec prev-y)]
                          :W [(dec prev-x) prev-y])]
    (conj coordinates next-coordinate)))

(defn coordinate-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn input->steps [input]
  (->> input
       (map parse-instruction)
       (reduce walk-instruction [[:N 0]])
       (drop 1)
       (reduce step-to-coordinate [[0 0]])))

(defn part-1 [input]
  (->> (input->steps input)
       last
       coordinate-distance))

(defn part-2 [input]
  (->> (input->steps input)
       u/first-duplicate
       coordinate-distance))

(comment
  (= 332 (part-1 input))
  (= 166 (part-2 input)))
