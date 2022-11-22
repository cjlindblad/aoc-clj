(ns aoc-clj.2016.07.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split-lines (slurp "src/aoc_clj/2016/07/input.txt")))

(defn parse-ip [line]
  (let [supernet (s/split line #"\[.*?\]")
        hypernet (re-seq #"\[.*?\]" line)]
    {:supernet supernet
     :hypernet hypernet}))

(defn abba? [s]
  (let [match
        (ffirst (re-seq #"([a-z])([a-z])\2\1" s))]
    (and match (not= (first match) (second match)))))

(defn with-abba [{:keys [supernet hypernet] :as ip}]
  (-> ip
      (assoc :supernet-abba (some abba? supernet))
      (assoc :hypernet-abba (some abba? hypernet))))

(defn tls? [{:keys [supernet-abba hypernet-abba]}]
  (and supernet-abba
       (not hypernet-abba)))

(defn part-1 [input]
  (->> (map parse-ip input)
       (map with-abba)
       (filter tls?)
       count))

(defn abas [s]
  (let [groups (partition 3 1 s)]
    (map #(apply str %) (filter #(and (= (first %) (last %)) (not= (first %) (second %))) groups))))

(defn invert-aba [s]
  (str (second s) (first s) (second s)))

(defn ssl? [{:keys [supernet hypernet]}]
  (let [supernet-abas (filter identity (mapcat abas supernet))
        hypernet-abas (map invert-aba (filter identity (mapcat abas hypernet)))
        common (set/intersection (apply hash-set supernet-abas) (apply hash-set hypernet-abas))]
    (< 0 (count common))))

(defn part-2 [input]
  (->> (map parse-ip input)
       (filter ssl?)
       count))

(comment
  (= 110 (part-1 input))
  (= 242 (part-2 input)))

