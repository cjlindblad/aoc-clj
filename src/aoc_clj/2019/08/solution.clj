(ns aoc-clj.2019.08.solution
  (:require [clojure.string :as string]))

(defn parse-int [n] (Integer/parseInt n))

(def input (string/trim (slurp "src/aoc_clj/2019/08/input.txt")))

(defn parse-pixels [input]
  (map (comp parse-int str) input))

(defn pixels->layers [pixels width height]
  (let [layer-pixels (* width height)
        layers (partition layer-pixels pixels)]
    layers))

(defn pixel-count [pixel-value layer]
  (count (filter #(= pixel-value %) layer)))

(defn part-1 [input]
  (let [pixels (parse-pixels input)
        layers (pixels->layers pixels 25 6)
        with-fewest-zeroes (first (sort-by (partial pixel-count 0) layers))]
    (*
     (pixel-count 1 with-fewest-zeroes)
     (pixel-count 2 with-fewest-zeroes))))

(defn first-opaque-pixel [pixels]
  (first (drop-while (fn [pixel] (= pixel 2)) pixels)))

(defn layers->stacks [layers]
  (apply map vector layers))

(defn layer-string [layer width]
  (->> (map {0 " " 1 "#"} layer)
       (apply str)
       (partition width)
       (map (fn [xs] (apply str xs)))))

(defn part-2 [input]
  (let [pixels (parse-pixels input)
        layers (pixels->layers pixels 25 6)
        stacks (layers->stacks layers)
        opaque-pixels (map first-opaque-pixel stacks)]
    (layer-string opaque-pixels 25)))

(comment
  (= 2440 (part-1 input))
  ;; run (part-2 input) to see result
  )

