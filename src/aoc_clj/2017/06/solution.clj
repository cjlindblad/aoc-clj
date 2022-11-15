(ns aoc-clj.2017.06.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (mapv parse-int (s/split (slurp "src/aoc_clj/2017/06/input.txt") #"\s+")))

(def test-input [0 2 7 0])

(defn most-blocks-index [memory]
  (let [max-block (apply max memory)]
    (.indexOf memory max-block)))

(defn distribute-blocks
  ([memory]
   (let [current-index (most-blocks-index memory)]
     (distribute-blocks
      (assoc memory current-index 0)
      (inc current-index)
      (get memory current-index))))
  ([memory index blocks]
   (if (zero? blocks) memory
       (let [mod-index (mod index (count memory))
             blocks-at-index (get memory mod-index)]
         (recur
          (assoc memory mod-index (inc blocks-at-index))
          (inc index)
          (dec blocks))))))

(defn find-first-duplicate
  ([memory] (find-first-duplicate memory #{} 1))
  ([memory found-configs n]
   (let [next-config (distribute-blocks memory)]
     (if (found-configs next-config) [next-config n]
         (recur next-config (conj found-configs next-config) (inc n))))))

(defn find-second-duplicate
  ([memory] (find-second-duplicate memory {} 1))
  ([memory found-configs n]
   (let [next-config (distribute-blocks memory)]
     (if (= 2 (found-configs next-config)) [next-config n]
         (recur
          next-config
          (if (found-configs next-config) (assoc found-configs next-config (inc (found-configs next-config)))
              (assoc found-configs next-config 1))
          (inc n))))))

(defn part-1 [input]
  (-> (find-first-duplicate input)
      last))

(defn part-2 [input]
  (let [first-cycle (last (find-first-duplicate input))
        second-cycle (last (find-second-duplicate input))]
    (- second-cycle first-cycle)))

(comment
  (= 11137 (part-1 input))
  (= 1037 (part-2 input)))

