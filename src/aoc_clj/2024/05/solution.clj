(ns aoc-clj.2024.05.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/05/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/05/test-input.txt"))

(defn parse-input [input]
  (let [[raw-rules raw-lines] (str/split input #"\n\n")
        lines (->> (str/split raw-lines #"\n")
                   (map #(str/split % #","))
                   (map (partial mapv parse-long)))
        rules (->> (str/split raw-rules #"\n")
                   (map #(str/split % #"\|"))
                   (map (partial mapv parse-long)))]
    {:rules rules
     :lines lines}))

(defn valid-rule? [line rule]
  (let [filtered (filter (into #{} rule) line)]
    (or (< (count filtered) 2)
        (= filtered rule))))

(defn all-rules-valid? [rules line]
  (->> (map (partial valid-rule? line) rules)
       (every? true?)))

(defn middle [xs]
  (let [length (count xs)
        half (/ (dec length) 2)]
    (-> (drop half xs) first)))

(defn part-1 [input]
  (let [{:keys [rules lines]} (parse-input input)
        valid-lines (filter (partial all-rules-valid? rules) lines)
        middles (map middle valid-lines)]
    (reduce + middles)))

(defn correct [line [left-rule right-rule]]
  (let [left-index (.indexOf line left-rule)
        right-index (.indexOf line right-rule)]
    (if (or (neg? left-index) (neg? right-index) (< left-index right-index))
      line
      (-> (assoc line left-index right-rule)
          (assoc right-index left-rule)))))

(defn apply-rules [line all-rules]
  (loop [line line rules all-rules]
    (if (seq rules)
      (recur (correct line (first rules)) (rest rules))
      (if (all-rules-valid? all-rules line)
        line
        (apply-rules line all-rules)))))

(defn part-2 [input]
  (let [{:keys [rules lines]} (parse-input input)
        invalid-lines (filter (complement (partial all-rules-valid? rules)) lines)
        corrected-lines (map (fn [line] (apply-rules line rules)) invalid-lines)
        middles (map middle corrected-lines)]
    (reduce + middles)))

(comment
  (= 5732 (part-1 input))
  (= 4716 (part-2 input)))


