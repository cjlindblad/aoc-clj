(ns aoc-clj.2024.11.solution)

(def input (read-string (str "[" (slurp "src/aoc_clj/2024/11/input.txt") "]")))
(def test-input [125 17])

(defn parse-long [n] (Long/parseLong n))

(defn apply-rule [stone]
  (let [stone-str (str stone)
        length (count stone-str)
        even-digits (even? length)]
    (cond
      (zero? stone) [1]
      even-digits (let [[left right] (partition (/ length 2) stone-str)]
                    [(parse-long (apply str left)) (parse-long (apply str right))])
      :else [(* stone 2024)])))

(defn stones->stone-count [stones]
  (->> (group-by identity stones)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

(defn solver [times stones]
  (->>  (loop [times times current-atom (atom (stones->stone-count stones))]
          (if (zero? times)
            @current-atom
            (let [next-atom (atom {})]
              (doseq [[stone stone-count] @current-atom]
                (let [expanded (apply-rule stone)]
                  (doseq [next-stone expanded]
                    (swap! next-atom update next-stone (fnil + 0) stone-count))))
              (recur (dec times) next-atom))))
        (map last)
        (reduce +)))

(def part-1 (partial solver 25))
(def part-2 (partial solver 75))

(comment
  (= 185894 (part-1 input))
  (= 221632504974231 (part-2 input)))

