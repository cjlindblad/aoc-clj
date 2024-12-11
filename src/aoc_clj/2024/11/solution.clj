(ns aoc-clj.2024.11.solution)

(def input (read-string (str "[" (slurp "src/aoc_clj/2024/11/input.txt") "]")))
(def test-input [125 17])

(defn digits [n]
  (loop [n n i 0]
    (if (zero? n)
      i
      (recur (quot n 10) (inc i)))))

(defn pow [b e]
  (loop [n b e (dec e)]
    (if (zero? e)
      n
      (recur (* n b) (dec e)))))

(defn split [n digits]
  (let [factor (pow 10 (/ digits 2))
        left (quot n factor)
        right (- n (* left factor))]
    [left right]))

(defn apply-rule [stone]
  (let [length (digits stone)
        even-digits (even? length)]
    (cond
      (zero? stone) [1]
      even-digits (let [[left right] (split stone length)]
                    [left right])
      :else [(* stone 2024)])))

(defn stones->stone-count [stones]
  (->> (group-by identity stones)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

(defn solver [times stones]
  (->> (let [stone-count-atom (atom (stones->stone-count stones))]
         (loop [times times stone-count-atom stone-count-atom]
           (let [next-atom (atom {})]
             (if (zero? times)
               stone-count-atom
               (do
                 (dorun
                  (map
                   (fn [[stone stone-count]]
                     (let [expanded (apply-rule stone)]
                       (dorun
                        (map
                         (fn [next-stone]
                           (swap! next-atom update next-stone (fnil + 0) stone-count))
                         expanded))))
                   @stone-count-atom))
                 (recur (dec times) next-atom))))))
       deref
       (map last)
       (reduce +)))

(def part-1 (partial solver 25))
(def part-2 (partial solver 75))

(comment
  (= 185894 (part-1 input))
  (= 221632504974231 (part-2 input)))

