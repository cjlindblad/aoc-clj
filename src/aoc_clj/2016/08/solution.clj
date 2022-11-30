(ns aoc-clj.2016.08.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2016/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2016/08/test-input.txt")))

(defn parse-instruction [input]
  (if-let [[[_ width height]] (re-seq #"rect (\d+)x(\d+)" input)]
    {:type :rect
     :width (Integer/parseInt width)
     :height (Integer/parseInt height)}
    (if-let [[[_ x val]] (re-seq #"rotate column x=(\d+) by (\d+)" input)]
      {:type :rotate-col
       :x (Integer/parseInt x)
       :val (Integer/parseInt val)}
      (if-let [[[_ y val]] (re-seq #"rotate row y=(\d+) by (\d+)" input)]
        {:type :rotate-row
         :y (Integer/parseInt y)
         :val (Integer/parseInt val)}))))

(defn rotate [xs n]
  (concat (take-last n xs) (take (- (count xs) n) xs)))

(defn make-coords [width height]
  (for [y (range 0 height) x (range 0 width)] [x y]))

(defn empty-grid [width height]
  (let [coords (make-coords width height)
        coords-map (zipmap coords (repeat false))]
    (merge coords-map {:width width :height height})))

(defn light-rect [grid width height]
  (let [coords (make-coords width height)
        coords-map (zipmap coords (repeat true))]
    (merge grid coords-map)))

(defn rotate-col [grid x val]
  (let [coords (for [y (range 0 (:height grid))] [x y])
        vals (map grid coords)
        rotated-vals (rotate vals val)
        next-coords-map (zipmap coords rotated-vals)]
    (merge grid next-coords-map)))

(defn rotate-row [grid y val]
  (let [coords (for [x (range 0 (:width grid))] [x y])
        vals (map grid coords)
        rotated-vals (rotate vals val)
        next-coords-map (zipmap coords rotated-vals)]
    (merge grid next-coords-map)))

(defn lit-pixels [grid]
  (count (filter true? (vals grid))))

(defn grid->string
  [grid]
  (let [coords (make-coords (:width grid) (:height grid))
        char-map {false \. true \#}]
    (->> (map grid coords)
         (map char-map)
         (partition (:width grid))
         (map #(apply str %)))))

(defn apply-instruction [grid instruction]
  (case (:type instruction)
        :rect (light-rect grid (:width instruction) (:height instruction))
        :rotate-col (rotate-col grid (:x instruction) (:val instruction))
        :rotate-row (rotate-row grid (:y instruction) (:val instruction))
        ))

(defn part-1 [input]
  (let [instructions (map parse-instruction input)
        grid (empty-grid 50 6)
        final-grid (reduce apply-instruction grid instructions)]
    (lit-pixels final-grid)))

(defn part-2 [input]
  (let [instructions (map parse-instruction input)
        grid (empty-grid 50 6)
        final-grid (reduce apply-instruction grid instructions)]
    (grid->string final-grid)))

(comment
  (= 110 (part-1 input))
  ; run (part-2 input) to see part 2 result
  )
