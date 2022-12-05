(ns aoc-clj.2016.09.solution
  (:require [clojure.string :as string]))

(def input (string/trim (slurp "src/aoc_clj/2016/09/input.txt")))

(defn decompress
  ([input] (decompress input ""))
  ([input result]
   (if-not (seq input) result
           (let [[[marker chars-str times-str]] (re-seq #"^\((\d+)x(\d+)\)" input)]
             (if-not marker (decompress (subs input 1) (str result (subs input 0 1)))
                     (let [chars (Integer/parseInt chars-str)
                           times (Integer/parseInt times-str)
                           remaining-input (subs input (count marker))
                           decompressed-marker (apply str (repeat times (subs remaining-input 0 chars)))
                           input-after-marker-chars (subs remaining-input chars)]
                       (decompress
                        input-after-marker-chars
                        (str result decompressed-marker))))))))

(defn part-1 [input]
  (count (decompress input)))

(defn decompress-2
  ([input] (decompress-2 input 0))
  ([input result]
   (if-not (seq input) result
           (let [[[marker chars-str times-str]] (re-seq #"^\((\d+)x(\d+)\)" input)]
             (if-not marker (recur (subs input 1) (inc result))
                     (let [chars (Integer/parseInt chars-str)
                           times (Integer/parseInt times-str)
                           remaining-input (subs input (count marker))
                           decompress-marker (apply str (repeat times (subs remaining-input 0 chars)))
                           input-after-marker-chars (subs remaining-input chars)]
                       (recur
                        (str decompress-marker input-after-marker-chars)
                        result
                        )))))))

(defn part-2 [input]
  (decompress-2 input))

(part-2 input)

(map part-2 ["X(8x2)(3x3)ABCY"
             "(27x12)(20x12)(13x14)(7x10)(1x12)A"
             "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"])

(comment
  (= 70186 (part-1 input)))
