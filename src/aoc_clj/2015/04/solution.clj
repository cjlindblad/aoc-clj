(ns aoc-clj.2015.04.solution
  (:require [aoc-clj.utils :as utils]))

(def input "ckczppom")

(defn find-suffix
  ([input regex] (find-suffix input regex 1))
  ([input regex n]
   (let [hash (utils/md5 (str input n))]
     (if (re-seq regex hash) n
         (recur input regex (inc n))))))

(defn part-1 [input]
  (find-suffix input #"^00000"))

(defn part-2 [input]
  (find-suffix input #"^000000"))

(comment
  (= 117946 (part-1 input))
  (= 3938038 (part-2 input)))

