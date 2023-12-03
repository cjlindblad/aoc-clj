(ns aoc-clj.2022.15.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/15/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/15/test-input.txt")))

(defn parse-instruction [line]
  (let [[sensor-x sensor-y closest-x closest-y]
        (->> (map (comp #(Integer/parseInt %) last) (re-seq #"([x|y]=(\-?\d+))" line)))]
    {:sensor-x sensor-x
     :sensor-y sensor-y
     :closest-x closest-x
     :closest-y closest-y}))

(defn manhattan-distance [x1 y1 x2 y2]
  (let [x (Math/abs (- x1 x2))
        y (Math/abs (- y1 y2))]
    (+ x y)))

(defn expand-occupied-coordinates [origin-x origin-y distance target-y]
  (if ((set (range (- origin-y distance) (inc (+ origin-y distance)))) target-y)
    (for [x (range (- origin-x distance) (inc (+ origin-x distance)))
          :when (>= distance (manhattan-distance x target-y origin-x origin-y))]
      [x target-y])
    []))

(defn instruction->occupied-coords [target-y {:keys [sensor-x sensor-y closest-x closest-y]}]
  (expand-occupied-coordinates sensor-x sensor-y (manhattan-distance sensor-x sensor-y closest-x closest-y) target-y))

(defn solver [row input]
  (let [instructions (map parse-instruction input)
        beacons (set (map (fn [{:keys [closest-x closest-y]}] [closest-x closest-y]) instructions))
        occupied-row-coordinates (set (mapcat (partial instruction->occupied-coords row) instructions))]
    (count (set/difference occupied-row-coordinates beacons))))

(def test-part-1 (partial solver 10))
(def part-1 (partial solver 2000000))

(test-part-1 test-input)
;; => 26

;; MOVE TO UTIL
(defn compute-line [[pt1 pt2]]
  (let [[x1 y1] pt1
        [x2 y2] pt2
        m (/ (- y2 y1) (- x2 x1))]
    {:slope  m
     :offset (- y1 (* m x1))}))

(defn intercept [line1 line2]
  (let [divisor (- (:slope  line2) (:slope  line1))]
    (if (zero? divisor) nil
        (let [x (/ (- (:offset line1) (:offset line2))
                   divisor)]
          {:x x
           :y (+ (* (:slope line1) x)
                 (:offset line1))}))))

(defn det [[x1 y1] [x2 y2]]
  (- (* x1 y2) (* y1 x2)))

(defn line-intersection [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (let [x-diff [(- x1 x2) (- x3 x4)]
        y-diff [(- y1 y2) (- y3 y4)]
        div (det x-diff y-diff)]
    (if (zero? div) nil
        (let [d [(det [x1 y1] [x2 y2]) (det [x3 y3] [x4 y4])]
              x (/ (det d x-diff) div)
              y (/ (det d y-diff) div)]
          [x y]))))

(line-intersection [[0 0] [4 4]] [[0 4] [4 0]])

(defn candidate-lines [[x y] distance]
  (let [right [(+ x distance) y]
        top [x (- y distance)]
        left [(- x distance) y]
        bottom [x (+ distance y)]]
    [[right top]
     [top left]
     [left bottom]
     [bottom right]]))

(defn instruction->candidate-lines [{:keys [sensor-x sensor-y closest-x closest-y]}]
  (candidate-lines [sensor-x sensor-y] (manhattan-distance sensor-x sensor-y closest-x closest-y)))

(defn get-candidate [a b]
  (for [x a y b :when (not= x y)]
    (line-intersection x y)))

(defn part-2 [input]
  (let [instructions (map parse-instruction input)
        sensors-with-distance (map (fn [{:keys [sensor-x sensor-y closest-x closest-y]}]
                                     {:sensor-x sensor-x :sensor-y sensor-y :distance (manhattan-distance sensor-x sensor-y closest-x closest-y)}) instructions)
        candidate-lines (map instruction->candidate-lines instructions)
        ;; lines (map (partial map compute-line) candidate-lines)
        candidates (mapcat identity (for [a candidate-lines b candidate-lines :when (not= a b)]
                                      (get-candidate a b)))
        filtered (filter (fn [{:keys [x y] :as point}] (and (not= nil point) (int? x) (int? y) (<= 0 x 20) (<= 0 y 20))) candidates)
        goal (filter (fn [{:keys [x y]}]
                       (not-any? (fn [{:keys [sensor-x sensor-y distance]}]
                                   (<= (manhattan-distance x y sensor-x sensor-y) distance)) sensors-with-distance)) filtered)]
    instructions
    sensors-with-distance
    candidate-lines
    goal
    candidates
    filtered
    ))

(part-2 test-input)

(filter (fn [{:keys [x y]}] (and (= x 14) (= y 11)))(part-2 test-input))

(comment
  (= 5511201 (part-1 test-input)))

