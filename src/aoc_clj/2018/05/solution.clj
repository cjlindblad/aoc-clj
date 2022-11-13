(ns aoc-clj.2018.05.solution
  (:require [clojure.string :as s]))

(def input (slurp "src/aoc_clj/2018/05/input.txt"))
(def test-input "dabAcCaCBAcCcaDA")

(def units
  ["a|A"
   "b|B"
   "c|C"
   "d|D"
   "e|E"
   "f|F"
   "g|G"
   "h|H"
   "i|I"
   "j|J"
   "k|K"
   "l|L"
   "m|M"
   "n|N"
   "o|O"
   "p|P"
   "q|Q"
   "r|R"
   "s|S"
   "t|T"
   "u|U"
   "v|V"
   "w|W"
   "x|X"
   "y|Y"
   "z|Z"])

(def polymer-re #"aA|Aa|bB|Bb|cC|Cc|dD|Dd|eE|Ee|fF|Ff|gG|Gg|hH|Hh|iI|Ii|jJ|Jj|kK|Kk|lL|Ll|mM|Mm|nN|Nn|oO|Oo|pP|Pp|qQ|Qq|rR|Rr|sS|Ss|tT|Tt|uU|Uu|vV|Vv|wW|Ww|xX|Xx|yY|Yy|zZ|Zz")

(defn react-polymer [polymer]
  (s/replace polymer polymer-re ""))

(defn fully-react-polymer [polymer]
  (let [next-polymer (react-polymer polymer)]
    (if (= polymer next-polymer) polymer
        (recur next-polymer))))

(defn part-1 [input]
  (->> input
       s/trim
       fully-react-polymer
       count))

(defn part-2 [input]
  (let [polymer (s/trim input)
        variants (map (fn [unit] (fully-react-polymer (s/replace polymer (re-pattern unit) ""))) units)]
    (count (first (sort-by count variants)))))

(comment
  (= 10978 (part-1 input))
  (= 4840 (part-2 input)))
