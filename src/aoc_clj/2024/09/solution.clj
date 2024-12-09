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
        memory-reversed-without-holes (filter (complement #{\.}) (reverse memory))]
    (-> (loop [memory memory rev memory-reversed-without-holes result []]
          (if (= (count result) (count memory-reversed-without-holes))
            result
            (if (not= \. (first memory))
              (recur (rest memory) rev (conj result (first memory)))
              (recur (rest memory) (rest rev) (conj result (first rev))))))
        checksum)))

(defn fit-thing [block thing]
  [thing (drop (count thing) block)])

(defn place-thing [memory thing]
  (->
   (reduce
    (fn [[result placed? passed?] block]
      (cond
        passed? [(conj result block) placed? passed?]
        (and placed? (= block thing)) [(conj result (repeat (count thing) \.)) placed? passed?]
        (= block thing)  [(conj result block) placed? true]
        (and (not placed?) (some #{\.} block) (<= (count thing) (count block))) [(apply conj result (fit-thing block thing)) true passed?]
        :else [(conj result block) placed? passed?]))
    [[] false false]
    memory)
   first))

(defn checksum-2 [memory]
  (->> (map-indexed (fn [idx cell] [idx cell]) memory)
       (filter (fn [[_ cell]] (not= cell \.)))
       (map (fn [[idx cell]] (* idx cell)))
       (reduce +)))

(defn part-2 [input]
  (let [memory (map-memory (parse-numbers input))
        memory-reversed-without-holes (filter (complement #{\.}) (reverse (map-memory (parse-numbers input))))
        memory-chunked (partition-by identity memory)
        rev-chunked (partition-by identity memory-reversed-without-holes)]
    (-> (reduce
         (fn [result thing] (place-thing result thing))
         memory-chunked
         rev-chunked)
        flatten
        checksum-2)))

(comment
  (= 6330095022244 (part-1 input))
  (= 6359491814941 (part-2 input)))


