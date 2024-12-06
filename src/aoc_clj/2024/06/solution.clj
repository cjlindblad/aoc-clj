(ns aoc-clj.2024.06.solution
  (:require [clojure.string :as str]))

(def input (str/split (slurp "src/aoc_clj/2024/06/input.txt") #"\n"))
(def test-input (str/split (slurp "src/aoc_clj/2024/06/test-input.txt") #"\n"))

(defn starting-pos [area]
  (let [max-y (count area)
        max-x (count (first area))]
    (first
     (for [y (range max-y)
           x (range max-x)
           :when (= \^ (get-in area [y x]))]
       {:x x :y y :direction :N}))))

(defn direction-delta [direction]
  (case direction
    :N [0 -1]
    :E [1 0]
    :S [0 1]
    :W [-1 0]))

(defn next-pos [{:keys [x y direction]}]
  (let [[dx dy] (direction-delta direction)]
    [(+ dx x) (+ dy y)]))

(defn blocked? [area pos]
  (let [[next-x next-y] (next-pos pos)]
    (-> (get-in area [next-y next-x])
        (= \#))))

(defn turn-right [direction]
  (direction {:N :E, :E :S, :S :W, :W :N}))

(defn inside? [area [x y]]
  (let [max-y (dec (count area))
        max-x (dec (count (first area)))]
    (and (<= 0 x max-x) (<= 0 y max-y))))

(defn move [area {:keys [x y direction inside] :as pos}]
  (let [blocked (blocked? area pos)
        next-direction (if blocked (turn-right direction) direction)
        really-blocked (blocked? area (assoc pos :direction next-direction))
        final-direction (if really-blocked (turn-right next-direction) next-direction)
        [next-x next-y] (next-pos {:x x :y y :direction final-direction})]
    {:x next-x :y next-y :direction final-direction :inside (inside? area [next-x next-y])}))

(defn walk [area pos]
  (loop [current-pos pos visited [pos]]
    (let [npos (move area current-pos)]
      (if (not (:inside npos))
        visited
        (recur npos (conj visited npos))))))

(defn distinct-locations [positions]
  (->> (map (juxt :x :y) positions)
       (into #{})
       count))

(defn part-1 [input]
  (-> (walk input (starting-pos input))
      distinct-locations))

(defn loop? [area pos]
  (loop [current-pos pos visited #{pos}]
    (let [npos (move area current-pos)]
      (if (not (:inside npos))
        false
        (if (visited npos) true
            (recur npos (conj visited npos)))))))

(defn part-2 [input]
  (let [positions (rest (walk input (starting-pos input)))
        area (mapv (partial mapv identity) input)
        candidates (distinct (map (fn [{:keys [x y]}] (assoc-in area [y x] \#)) positions))
        start-pos (starting-pos input)
        ;; we could be smarter about starting position here
        loops (filter (fn [candidate] (loop? candidate (assoc start-pos :inside true))) candidates)]
    (count loops)))

(comment
  (= 4977 (part-1 input))
  (= 1729 (part-2 input)))

