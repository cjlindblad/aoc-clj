(ns aoc-clj.2015.08.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2015/08/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2015/08/test-input.txt")))

(defn escaped-count [string]
  (-> (s/replace string #"\\\\" "!")
      (s/replace #"\\\"" "\"")
      (s/replace #"\\x.." "!")
      count
      (- 2)))

(defn expand-escape [string]
  (-> (s/replace string #"\"" "!!")
      (s/replace #"\\" "!!")
      count
      (+ 2)))

(defn part-1 [input]
  (let [literal-characters (map count input)
        memory-characters (map escaped-count input)]
    (- (reduce + literal-characters)
       (reduce + memory-characters))))

(defn part-2 [input]
  (let [literal-characters (map count input)
        expanded-characters (map expand-escape input)]
    (- (reduce + expanded-characters)
       (reduce + literal-characters))))

(comment
  (= 1333 (part-1 input))
  (= 2046 (part-2 input)))

