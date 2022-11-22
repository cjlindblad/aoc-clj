(ns aoc-clj.2018.07.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split-lines (slurp "src/aoc_clj/2018/07/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2018/07/test-input.txt")))

(defn parse-instruction [line]
  (let [[before after] (re-seq #" [A-Z] " line)]
    {:before (s/trim before)
     :after (s/trim after)}))

(defn build-before->after
  ([instructions] (build-before->after instructions {}))
  ([instructions result]
   (if-not (seq instructions) result
           (let [{:keys [before after]} (first instructions)
                 next-result (if-let [existing-before (result before)]
                               (assoc result before (conj existing-before after))
                               (assoc result before [after]))]
             (recur (rest instructions) next-result)))))

(defn build-after->before
  ([instructions] (build-after->before instructions {}))
  ([instructions result]
   (if-not (seq instructions) result
           (let [{:keys [before after]} (first instructions)
                 next-result (if-let [existing-after (result after)]
                               (assoc result after (conj existing-after before))
                               (assoc result after [before]))]
             (recur (rest instructions) next-result)))))

(defn find-roots [after->before]
  (let [key-set (apply hash-set (keys after->before))
        val-set (apply hash-set (flatten (vals after->before)))]
    (into [] (set/difference val-set key-set))))

(defn count-nodes [before->after after->before]
  (count (apply hash-set (concat (keys before->after) (keys after->before)))))

(defn calculate-order
  ([before->after after->before roots node-count] (calculate-order before->after after->before (sort roots) [] node-count))
  ([before->after after->before available result node-count]
   (if (= node-count (count result)) result
       (let [result-set (apply hash-set result)
             next-node (->> available
                            (filter (fn [n] (set/subset? (apply hash-set (after->before n)) result-set)))
                            first)
             next-available (sort (distinct (concat (before->after next-node) (filter (fn [n] (not= n next-node)) available))))
             next-result (conj result next-node)]
         (recur before->after after->before next-available next-result node-count)))))

(defn part-1 [input]
  (let [instructions (map parse-instruction input)
        before->after (build-before->after instructions)
        after->before (build-after->before instructions)
        roots (find-roots after->before)
        node-count (count-nodes before->after after->before)
        order (calculate-order before->after after->before roots node-count)]
    (apply str order)))

(defn letter-range [from to]
  (map (comp str char) (range (int from) (inc (int to)))))

(defn initial-step-time [step]
  (let [letters (letter-range \A \Z)
        seconds (range 1 (inc 26))
        letters->seconds (zipmap letters seconds)]
    (+ 60
       (letters->seconds step))))

(defn part-2-solver
  ([before->after after->before roots node-count] (part-2-solver before->after after->before (sort roots) [] node-count -1 []))
  ([before->after after->before available result node-count seconds workers]
   (if (= node-count (count result)) (inc seconds)
       (let [result-set (apply hash-set result)
             available-workers (- 5 (count workers))
             next-nodes (->> available
                             (filter (fn [n] (set/subset? (apply hash-set (after->before n)) result-set)))
                             (into [])
                             sort
                             (take available-workers))
             added-workers (concat workers (map (fn [n] [n (initial-step-time n)]) next-nodes))
             worked-workers (map (fn [[n seconds]] [n (dec seconds)]) added-workers)
             completed-steps (map first (filter (fn [[_ seconds]] (zero? seconds)) worked-workers))
             next-workers (filter (fn [[_ seconds]] (< 0 seconds)) worked-workers)
             worked-on (apply hash-set (map first next-workers))
             next-result (concat result completed-steps)
             unlocked-available (mapcat before->after completed-steps)
             next-available (sort (distinct (filter (fn [n] (not (worked-on n))) (concat unlocked-available (filter (fn [n] (not ((apply hash-set completed-steps) n))) available)))))]
         (recur before->after after->before next-available next-result node-count (inc seconds) next-workers)))))

(defn part-2 [input]
  (let [instructions (map parse-instruction input)
        before->after (build-before->after instructions)
        after->before (build-after->before instructions)
        roots (find-roots after->before)
        node-count (count-nodes before->after after->before)]
    (part-2-solver before->after after->before roots node-count)))

(comment
  (= "BITRAQVSGUWKXYHMZPOCDLJNFE" (part-1 input))
  (= 869 (part-2 input)))

