(ns aoc-clj.2024.09.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/09/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/09/test-input.txt"))

(defn parse-long [n]
  (Long/parseLong n))

(defn parse-numbers [input]
  (->> (str/split input #"")
       drop-last
       (mapv parse-long)))

(defn checksum [memory]
  (->> (map-indexed (fn [index value] (* index value)) memory)
       (reduce +)))

(defn map-memory [nums]
  (->> (map-indexed
        (fn [idx num]
          (let [id (/ idx 2)]
            (if (not (odd? idx))
              (repeat num id)
              (repeat num \.))))
        nums)
       flatten))

(defn part-1 [input]
  (let [memory (map-memory (parse-numbers input))
        memory-reversed-without-holes (filter (complement #{\.}) (reverse (map-memory (parse-numbers input))))]
    (-> (loop [memory memory rev memory-reversed-without-holes result []]
          (if (= (count result) (count memory-reversed-without-holes))
            result
            (if (not= \. (first memory))
              (recur (rest memory) rev (conj result (first memory)))
              (recur (rest memory) (rest rev) (conj result (first rev))))))
        checksum)))

(comment
  (= 6330095022244 (part-1 input)))


