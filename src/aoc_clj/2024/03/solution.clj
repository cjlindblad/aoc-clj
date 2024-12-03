(ns aoc-clj.2024.03.solution)

(def input (slurp "src/aoc_clj/2024/03/input.txt"))

(defn solver [muls]
  (->> (map (partial re-seq #"(\d+),(\d+)") muls)
       (map (partial mapcat (partial drop 1)))
       (map (partial map parse-long))
       (map (partial reduce *))
       (reduce +)))

(defn part-1 [input]
  (->>  (re-seq #"mul\(\d{1,3},\d{1,3}\)" input)
        solver)); 173785482

(defn part-2 [input]
  (->>
   (re-seq #"don\'t\(\)|do\(\)|mul\(\d{1,3},\d{1,3}\)" input)
   (reduce
    (fn [[skip result] substring]
      (let [is-trigger (#{"do()" "don't()"} substring)
            next-skip (case substring
                        "do()" false
                        "don't()" true
                        skip)
            next-result (if (or is-trigger next-skip) result (conj result substring))]
        [next-skip next-result]))
    [false []])
   last
   solver)); 83158140

(comment
  (= 173785482 (part-1 input))
  (= 83158140 (part-2 input)))
