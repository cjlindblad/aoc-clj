(ns aoc-clj.2016.04.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (s/split-lines (slurp "src/aoc_clj/2016/04/input.txt")))

(defn parse-room [line]
  {:letters (-> (s/split line #"\d+") first (s/replace "-" ""))
   :id (-> (re-seq #"\d+" line) first parse-int)
   :checksum (->> (re-seq #"\[\w+\]" line) first (drop 1) (drop-last 1) (apply str))})

(defn checksum [letters]
  (->>
   (group-by identity letters)
   (sort-by first)
   (sort-by #(count (last %)) #(> %1 %2))
   (map first)
   (take 5)
   (apply str)))

(defn valid-room? [room]
  (= (:checksum room) (checksum (:letters room))))

(defn part-1 [input]
  (->> input
       (map parse-room)
       (filter valid-room?)
       (map :id)
       (apply +)))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn rotate [alphabet offset]
  (zipmap
   alphabet
   (take (count alphabet) (drop offset (cycle alphabet)))))

(defn rotate-room [room]
  (let [rotation (rotate alphabet (:id room))]
    (conj room
          [:name
           (apply str (map rotation (:letters room)))])))

(defn part-2 [input]
  (->> input
       (map parse-room)
       (map rotate-room)
       (filter #(re-seq #"northpoleobject" (:name %)))
       first
       :id
       ))

(comment
  (= 278221 (part-1 input))
  (= 267 (part-2 input)))

