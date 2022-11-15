(ns aoc-clj.2018.06.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2018/06/input.txt")))
(def test-input
  ["1, 1"
   "1, 6"
   "8, 3"
   "3, 4"
   "5, 5"
   "8, 9"])

(defn parse-coord [line]
  (let [[x y] (s/split line #", ")]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn minmax [coords]
  {:min-y (->> coords (map second) (apply min))
   :max-y (->> coords (map second) (apply max))
   :min-x (->> coords (map first) (apply min))
   :max-x (->> coords (map first) (apply max))})

(defn make-grid [coords]
  (let [{:keys [min-y max-y min-x max-x]} (minmax coords)]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))]
      [x y]))
  )

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn closest-coord [coord coords]
  (let [distances (sort-by last (map (fn [c] [c (manhattan-distance c coord)]) coords))]
    (if (= (last (first distances)) (last (second distances))) nil
        (first (first distances)))))

(defn outer-ring-coords [coords]
  (let [{:keys [min-x min-y max-x max-y]} (minmax coords)]
    (for [x (range min-x (inc max-x))
          y (range min-y (inc max-y))
          :when (or (= x min-x)
                    (= x max-x)
                    (= y min-y)
                    (= y max-y))]
      [x y])))

(defn part-1 [input]
  (let [coords (map parse-coord input)
        ;; {:keys [min-x max-x min-y max-y]} (minmax coords)
        grid (make-grid coords)
        closest (filter (fn [[_ c]] (not= nil c)) (map (fn [c] [c (closest-coord c coords)]) grid))
        coord->closest (into {} closest)

        ;; TODO calculate which coordinates are in the outer ring and eliminate them
        outer-ring (outer-ring-coords coords)
        outer-closest (apply hash-set (filter (complement nil?) (map coord->closest outer-ring)))
        finite (filter (fn [[_ c]] ((complement outer-closest) c)) closest)
        closest-count-group (group-by last finite)]

    (last (sort (map count (map last closest-count-group))))
    ))

(defn total-manhattan-distance [coord coords]
  (->> (map #(manhattan-distance coord %) coords)
       (reduce +)))

(defn safe-distances
  ([grid coords] (safe-distances grid coords []))
  ([grid coords result]
   (if-not (seq grid) result
           (let [current-coord (first grid)
                 current-distance (total-manhattan-distance current-coord coords)
                 next-result (if (< current-distance 10000) (conj result current-distance) result)]
             (recur (rest grid) coords next-result)))))

(defn make-huge-grid [coords]
  (let [{:keys [min-y max-y min-x max-x]} (minmax coords)]
    (for [x (range (- min-x 500) (+ max-x 500))
          y (range (- min-y 500) (+ max-y 500))]
      [x y])))

(defn part-2 [input]
  (let [coords (map parse-coord input)
        grid (make-huge-grid coords)
        distances (safe-distances grid coords)]
    (count distances)))

(comment
  (= 4829 (part-1 input))
  (= 46966 (part-2 input))
  )
