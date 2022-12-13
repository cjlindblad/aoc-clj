(ns aoc-clj.2022.13.solution
  (:require [clojure.string :as string]))

(def input (slurp "src/aoc_clj/2022/13/input.txt"))
(def test-input (slurp "src/aoc_clj/2022/13/test-input.txt"))

(defn parse-packets [input]
  (->> (string/split input #"\n+")
       (map read-string)
       (map eval)))

(defn ensure-seqable [x]
  (if-not (seqable? x) [x] x))

(defn right-order? [[first-packets second-packets]]
  (cond (and (empty? first-packets) (empty? second-packets)) :continue
        (empty? first-packets) true
        (empty? second-packets) false
        :else (let [left (first first-packets)
                    right (first second-packets)
                    both-ints? (and (int? left) (int? right))
                    result (if both-ints?
                             (cond (< left right) true
                                   (> left right) false
                                   :else :continue)
                             (right-order? [(ensure-seqable left) (ensure-seqable right)]))]
                (if (= :continue result)
                  (recur [(rest first-packets) (rest second-packets)])
                  result))))

(defn part-1 [input]
  (let [packets (partition 2 (parse-packets input))]
    (->> (map right-order? packets)
         (map-indexed vector)
         (filter (fn [[_ right-order]] right-order))
         (map (comp inc first))
         (reduce +))))

(defn order-comparator [left right]
  (let [result (right-order? [left right])]
    (if result -1 1)))

(defn part-2 [input]
  (let [packets (parse-packets input)
        with-dividers (concat packets [[[2]] [[6]]])
        sorted (sort order-comparator with-dividers)]
    (->> (map-indexed vector sorted)
         (filter (fn [[_ v]] (#{[[2]] [[6]]} v)))
         (map (comp inc first))
         (reduce *))))

(comment
  (= 6086 (part-1 input))
  (= 27930 (part-2 input)))
