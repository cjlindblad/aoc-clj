(ns aoc-clj.2015.01.solution
  (:require [clojure.string :as str]))

(def input
  (slurp "src/aoc_clj/2015/01/input.txt"))

(def paren-mapping {"(" 1 ")" -1})

(defn parens-to-floors [parens]
  (->>
   (str/split parens #"")
   (map #(get paren-mapping %))
   (filter (complement nil?))
   (reduce
    (fn [floors direction]
      (cond
        (empty? floors) [direction]
        :else (conj floors (+ (last floors) direction))))
    [])))

(defn final-floor [parens]
  (->
   (parens-to-floors parens)
   last))

(defn first-position-of-floor [parens floor]
  (->
   (parens-to-floors parens)
   (.indexOf floor)
   inc))

(comment
  (= 232 (final-floor input))
  (= 1783 (first-position-of-floor input -1)))
