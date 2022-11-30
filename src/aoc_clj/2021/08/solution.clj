(ns aoc-clj.2021.08.solution
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input (string/split-lines (slurp "src/aoc_clj/2021/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2021/08/test-input.txt")))

(defn string->set [s] (apply hash-set s))

(defn parse-instruction [line]
  (let [[signal-patterns output-values] (string/split line #" \| ")]
    {:signal-patterns (map string->set (string/split signal-patterns #" "))
     :output-values (map string->set (string/split output-values #" "))}))

(defn parse-digit [segments]
  (case (count segments)
    2 1
    4 4
    3 7
    7 8
    nil))

(defn part-1 [input]
  (let [instructions (map parse-instruction input)
        output-values (map :output-values instructions)
        digits (map parse-digit (flatten output-values))]
    (count (filter (complement nil?) digits))))

(defn deduce-segments [signal-patterns]
  (let [count->letter (map (fn [[k v]] [k (count v)]) (group-by identity (mapcat vec signal-patterns)))
        b (ffirst (filter (fn [[_ v]] (= v 6)) count->letter))
        e (ffirst (filter (fn [[_ v]] (= v 4)) count->letter))
        f (ffirst (filter (fn [[_ v]] (= v 9)) count->letter))
        seven (first (filter (fn [p] (= (count p) 3)) signal-patterns))
        one (first (filter (fn [p] (= (count p) 2)) signal-patterns))
        a (first (set/difference seven one))
        c (ffirst (filter (fn [[k v]] (and (= v 8) (not= k a))) count->letter))
        g (first (set/difference (first (filter (fn [p] (and (= 6 (count p)) (set/subset? #{a b c e f} p))) signal-patterns)) #{a b c e f}))
        d (first (set/difference (first (filter (fn [p] (= 7 (count p))) signal-patterns)) #{b e f a c g}))]
    {b \b
     e \e
     f \f
     a \a
     c \c
     g \g
     d \d}))

(def numbers
  {#{\a \b \c \e \f \g} 0
   #{\c \f} 1
   #{\a \c \d \e \g} 2
   #{\a \c \d \f \g} 3
   #{\b \c \d \f} 4
   #{\a \b \d \f \g} 5
   #{\a \b \d \e \f \g} 6
   #{\a \c \f} 7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g} 9})

(defn convert-output-values [segments output-values]
  (map (fn [output] (apply hash-set (map segments output))) output-values))

(defn output-numbers [{:keys [signal-patterns output-values]}]
  (let [segments (deduce-segments signal-patterns)
        converted (convert-output-values segments output-values)]
    (Integer/parseInt (apply str (map numbers converted)))))

(defn part-2 [input]
  (let [instructions (map parse-instruction input)
        numbers (map output-numbers instructions)]
    (reduce + numbers)))

(comment
  (= 534 (part-1 input))
  (= 1070188 (part-2 input)))
