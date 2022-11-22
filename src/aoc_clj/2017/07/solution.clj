(ns aoc-clj.2017.07.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split-lines (slurp "src/aoc_clj/2017/07/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2017/07/test-input.txt")))

(defn parse-program [line]
  (let [name (first (re-seq #"^\w+" line))
        weight (Integer/parseInt (first (re-seq #"\d+" line)))
        sub-programs (last (first (re-seq #"(?: \-\> )(.+)" line)))]
    {:name name
     :weight weight
     :sub-programs (when sub-programs (s/split sub-programs #", "))}))

(defn build-tree
  ([programs] (build-tree programs {}))
  ([programs tree]
   (if-not (seq programs) tree
           (let [{:keys [name sub-programs]} (first programs)
                 entries (map (fn [sub] [sub name]) sub-programs)]
             (recur (rest programs) (into tree entries))))))

(defn build-weights
  ([programs] (build-weights programs {}))
  ([programs weights]
   (if-not (seq programs) weights
           (let [{:keys [name weight]} (first programs)]
             (recur (rest programs) (assoc weights name weight))))))

(defn root [tree]
  (first (set/difference (apply hash-set (vals tree)) (apply hash-set (keys tree)))))

(defn part-1 [input]
  (let [programs (map parse-program input)
        tree (build-tree programs)]
    (root tree)))

(defn program->subs [tree]
  (->> (group-by last tree)
       (map (fn [[k v]] [k (map first v)]))
       (into {})))

(defn build-nested-tree
  [root subs weights]
  (let [next-subs (subs root)
        weight (weights root)]
    (if-not next-subs [root weight]
            [[root weight] (mapv #(build-nested-tree % subs weights) next-subs)])))

(defn find-wrong-weight [nested-tree]
  (let [subs (second nested-tree)
        [root-name root-weight] (first nested-tree)]
    (if (number? subs) nested-tree
        (let [sub-weights (map find-wrong-weight subs)]
          (if-not (apply = (map last sub-weights))
            sub-weights
            [root-name
             (+ root-weight (reduce + (map last sub-weights)))])))))

(defn inner-seq [xs]
  (let [inner (first (drop-while (complement seq?) xs))]
    (if (seq? inner) (recur inner)
        xs)))

(defn find-correction [xs]
  (let [[wrong right] (->> (group-by last xs)
                           (sort-by (fn [[_ v]] (count v)))
                           (map (fn [[k v]] [(ffirst v) k])))]
    [(first wrong) (- (last right) (last wrong))]))

(defn part-2 [input]
  (let [programs (map parse-program input)
        tree (build-tree programs)
        subs (program->subs tree)
        root-program (root tree)
        weights (build-weights programs)
        nested-tree (build-nested-tree root-program subs weights)
        wrong-weight (find-wrong-weight nested-tree)
        inner (inner-seq wrong-weight)
        [name correction] (find-correction inner)]
    (+ (weights name) correction)))

(comment
  (= "vgzejbd" (part-1 input))
  (= 1226 (part-2 input)))
