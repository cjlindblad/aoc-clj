(ns aoc-clj.2017.04.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2017/04/input.txt")))
(defn valid-passphrase-1? [passphrase]
  (let [words (s/split passphrase #"\s+")
        words-set (apply hash-set words)]
    (= (count words) (count words-set))))

(defn part-1 [input]
  (->> input
       (filter valid-passphrase-1?)
       count))

(defn valid-passphrase-2? [passphrase]
  (let [words (s/split passphrase #"\s+")
        sorted-words (map sort words)
        sorted-words-set (apply hash-set sorted-words)]
    (= (count words) (count sorted-words-set))))

(defn part-2 [input]
  (->> input
       (filter valid-passphrase-2?)
       count))

(comment
  (= 325 (part-1 input))
  (= 119 (part-2 input)))
