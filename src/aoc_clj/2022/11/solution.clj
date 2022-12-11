(ns aoc-clj.2022.11.solution
  (:require [clojure.string :as string]))

(def input (string/split (slurp "src/aoc_clj/2022/11/input.txt") #"\n\n"))
(def test-input (string/split (slurp "src/aoc_clj/2022/11/test-input.txt") #"\n\n"))

(defn parse-monkey [input]
  (let [[monkey-line items-line op-line test-line true-line false-line] (string/split-lines input)]
    {:monkey (Integer/parseInt (first (re-seq #"\d" monkey-line)))
     :items (mapv #(Integer/parseInt %) (string/split (last (string/split items-line #": ")) #", "))
     :op (keyword (last (drop-last (string/split op-line #" "))))
     :op-arg (let [arg (last (string/split op-line #" "))]
               (if (#{"old"} arg) :old (Integer/parseInt arg)))
     :divisible-by (Integer/parseInt (first (re-seq #"\d+" test-line)))
     :true-monkey (Integer/parseInt (first (re-seq #"\d+" true-line)))
     :false-monkey (Integer/parseInt (first (re-seq #"\d+" false-line)))
     :inspections 0}))

(defn next-worry [item op op-arg]
  (let [f (case op
            :* *'
            :+ +')
        x (case op-arg
            :old item
            op-arg)]
    (f item x)))

(defn monkey-step
  ([monkeys rounds decrease-func] (monkey-step monkeys 0 1 rounds decrease-func))
  ([monkeys monkey-index round rounds decrease-func]
   (let [{:keys [items op op-arg divisible-by true-monkey false-monkey] :as monkey} (monkeys monkey-index)]
     (if (= (inc rounds) round) monkeys
         (if (empty? items)
           (let [next-index (mod (inc monkey-index) (count monkeys))
                 next-round (if (= monkey-index (dec (count monkeys))) (inc round) round)]
             (recur monkeys next-index next-round rounds decrease-func))
           (let [supermodulo (reduce * (map :divisible-by (vals monkeys)))
                 current-item (first items)
                 increased-worry (mod (next-worry current-item op op-arg) supermodulo)
                 decreased-worry (decrease-func increased-worry)
                 passed-test? (zero? (mod decreased-worry divisible-by))
                 pass-to-index (if passed-test? true-monkey false-monkey)
                 pass-to-monkey (monkeys pass-to-index)
                 next-current-monkey (update (assoc monkey :items (vec (drop 1 (:items monkey)))) :inspections inc)
                 next-pass-to-monkey (assoc pass-to-monkey :items (vec (conj (:items pass-to-monkey) decreased-worry)))
                 next-monkeys (merge monkeys {monkey-index next-current-monkey} {pass-to-index next-pass-to-monkey})]
             (recur next-monkeys monkey-index round rounds decrease-func)))))))

(defn solver [rounds decrease-func input]
  (let [monkeys (zipmap (range) (map parse-monkey input))
        result (monkey-step monkeys rounds decrease-func)]
    (->> (vals result)
         (map :inspections)
         sort
         (take-last 2)
         (reduce *))))

(def part-1 (partial solver 20 #(quot % 3)))
(def part-2 (partial solver 10000 identity))

(comment
  (= 61503 (part-1 input))
  (= 14081365540 (part-2 input)))
