(ns aoc-clj.2021.03.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2021/03/input.txt")))
input

(defn transpose [xs]
  (apply map vector xs))

(def bitmap
  {\1 :ones
   \0 :zeroes})

(defn grouped-count [xs]
  (->> (group-by identity xs)
       (mapcat (fn [[k v]] [(bitmap k) (count v)]))
       (apply hash-map)))

(defn most-common [xs]
  (let [{:keys [zeroes ones]} (grouped-count xs)]
    (cond
      (= zeroes ones) \1
      (> zeroes ones) \0
      (> ones zeroes) \1)))

(defn least-common [xs]
  (let [{:keys [zeroes ones]} (grouped-count xs)]
    (cond
      (= zeroes ones) \0
      (< zeroes ones) \0
      (< ones zeroes) \1)))

(defn part-1 [input]
  (let [columns (transpose input)
        most-common-bits (map most-common columns)
        least-common-bits (map least-common columns)
        gamma-rate (Integer/parseInt (apply str most-common-bits) 2)
        epsilon-rate (Integer/parseInt (apply str least-common-bits) 2)]
    (* gamma-rate epsilon-rate)))

(defn calculate-rating [bit-calculation rows index]
  (if (= 1 (count rows)) (first rows)
      (let [columns (transpose rows)
            bit (bit-calculation (nth columns index))
            remaining-rows (filter #(= bit (nth % index)) rows)]
        (recur
         bit-calculation
         remaining-rows
         (inc index)))))

(def calculate-oxygen-rating (partial calculate-rating most-common))
(def calculate-scrubber-rating (partial calculate-rating least-common))

(defn part-2 [input]
  (let [oxygen-rating (calculate-oxygen-rating input 0)
        scrubber-rating (calculate-scrubber-rating input 0)]
    (*
     (Integer/parseInt oxygen-rating 2)
     (Integer/parseInt scrubber-rating 2))))

(comment
  (= 4138664 (part-1 input))
  (= 4273224 (part-2 input)))
