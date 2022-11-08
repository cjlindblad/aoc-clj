(ns aoc-clj.2019.03.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (s/split-lines (slurp "src/aoc_clj/2019/03/input.txt")))

(defn parse-instruction [raw-instruction]
  (let [direction (-> (first raw-instruction) str keyword)
        value (->> (rest raw-instruction) (apply str) parse-int)]
    [direction value]))

(defn parse-row [row]
  (map parse-instruction row))

(defn get-next-line [prev-line [direction value]]
  (let [prev-x (:x2 prev-line)
        prev-y (:y2 prev-line)
        prev-length (:length prev-line)
        [delta-x delta-y] (case direction
                            :U [0 value]
                            :D [0 (* -1 value)]
                            :R [value 0]
                            :L [(* -1 value) 0])]
    {:x1 prev-x
     :y1 prev-y
     :x2 (+ prev-x delta-x)
     :y2 (+ prev-y delta-y)
     :length (+ prev-length value)}))

(defn row->lines
  ([row] (row->lines row []))
  ([row lines]
   (if (empty? row) lines
       (let [prev-line (if (empty? lines) {:x2 0 :y2 0 :length 0} (last lines))
             next-line (get-next-line prev-line (first row))]
         (recur (rest row) (conj lines next-line))))))

(defn line-orientation [line]
  (cond
    (= (:y1 line) (:y2 line)) :horizontal
    (= (:x1 line) (:x2 line)) :vertical))

(defn intersection-x-length [line x]
  (- (:length line) (- (max x (:x2 line)) (min x (:x2 line)))))

(defn intersection-y-length [line y]
  (- (:length line) (- (max y (:y2 line)) (min y (:y2 line)))))

(defn intersection [line-a line-b]
  (let [orientation-a (line-orientation line-a)
        orientation-b (line-orientation line-b)]
    (when (not= orientation-a orientation-b)
      (case (line-orientation line-a)
        :horizontal (let [sorted-xs-a (sort [(:x1 line-a) (:x2 line-a)])
                          sorted-ys-b (sort [(:y1 line-b) (:y2 line-b)])]
                      (when
                       (and
                        (< (first sorted-xs-a) (:x1 line-b) (last sorted-xs-a))
                        (< (first sorted-ys-b) (:y1 line-a) (last sorted-ys-b)))
                        [(:x1 line-b)
                         (:y1 line-a)
                         (+ (intersection-x-length line-a (:x1 line-b))
                            (intersection-y-length line-b (:y1 line-a)))]))
        :vertical (let [sorted-xs-b (sort [(:x1 line-b) (:x2 line-b)])
                        sorted-ys-a (sort [(:y1 line-a) (:y2 line-a)])]
                    (when
                     (and
                      (< (first sorted-xs-b) (:x1 line-a) (last sorted-xs-b))
                      (< (first sorted-ys-a) (:y1 line-b) (last sorted-ys-a)))
                      [(:x1 line-a)
                       (:y1 line-b)
                       (+ (intersection-y-length line-a (:y1 line-b))
                          (intersection-x-length line-b (:x1 line-a)))]))))))

(defn intersections [[a-lines b-lines]]
  (for [a a-lines b b-lines
        :when (intersection a b)]
    (intersection a b)))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn part-1 [input]
  (->> input
       (map #(s/split % #","))
       (map parse-row)
       (map row->lines)
       intersections
       (map manhattan-distance)
       sort
       first))

(defn part-2 [input]
  (->> input
       (map #(s/split % #","))
       (map parse-row)
       (map row->lines)
       intersections
       (map last)
       sort
       first))

(comment
  (= 1674 (part-1 input))
  (= 14012 (part-2 input)))
