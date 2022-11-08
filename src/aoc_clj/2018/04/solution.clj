(ns aoc-clj.2018.04.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2018/04/input.txt")))

(defn parse-input [line]
  (let [[_ timeish & info] (s/split line #" ")
        time (apply str (drop-last 1 timeish))
        minutes (Integer/parseInt (apply str (take-last 2 time)))
        guard-id (last (last (re-seq #"#(\d+)" (second info))))]
    {:minutes minutes
     :id guard-id
     :activity (s/join " " (take-last 2 info))}))

(defn group-by-shift
  ([entries] (group-by-shift entries []))
  ([entries groupings]
   (if (empty? entries) groupings
       (let [{:keys [id minutes activity]} (first entries)]
         (if id (recur (rest entries) (conj (vec groupings) [id []]))
             (let [last-grouping (last groupings)
                   updated-grouping [(first last-grouping) (conj (vec (last last-grouping)) {:minutes minutes :activity activity})]]
               (recur (rest entries) (conj (vec (drop-last 1 groupings)) updated-grouping))))))))

(defn group-by-guard [groupings]
  (->> (group-by first groupings)
       (map (fn [[k v]] [k (mapcat #(flatten (last %)) v)]))))

(defn grouping->sleep-minutes [[id activities]]
  (let [activity-pairs (partition 2 activities)
        minutes (mapcat (fn [[v1 v2]] (range (:minutes v1) (:minutes v2))) activity-pairs)
        sleepiest-time (->> (group-by identity minutes)
                            (sort-by #(count (last %)))
                            last)]
    {:id id
     :total-sleep (count minutes)
     :sleepiest-minute (first sleepiest-time)
     :sleepiest-count (count (last sleepiest-time))}))

(defn part-1 [input]
  (let [guard (->> input
                   sort
                   (map parse-input)
                   group-by-shift
                   group-by-guard
                   (map grouping->sleep-minutes)
                   (sort-by :total-sleep)
                   last)]
    (* (Integer/parseInt (:id guard)) (:sleepiest-minute guard))))

(defn part-2 [input]
  (let [guard (->> input
                   sort
                   (map parse-input)
                   group-by-shift
                   group-by-guard
                   (map grouping->sleep-minutes)
                   (sort-by :sleepiest-count)
                   last)]
    (* (Integer/parseInt (:id guard)) (:sleepiest-minute guard))))

(comment
  (= 106710 (part-1 input))
  (= 10491 (part-2 input)))

