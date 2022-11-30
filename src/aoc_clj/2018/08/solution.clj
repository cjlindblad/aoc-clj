(ns aoc-clj.2018.08.solution
  (:require [clojure.string :as string]))

(defn parse-int [n] (Integer/parseInt n))

(def input (string/trim (slurp "src/aoc_clj/2018/08/input.txt")))
(def test-input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")

(defn parse-instructions [input]
  (->> (string/split input #" ")
       (map parse-int)))

(declare parse-node)

(defn get-child-nodes
  ([numbers child-count] (get-child-nodes numbers child-count []))
  ([numbers child-count child-nodes]
   (if (= child-count (count child-nodes)) child-nodes
       (let [next-child (parse-node numbers)]
         (recur
          (drop (:node-size next-child) numbers)
          child-count
          (conj child-nodes next-child))))))

(defn parse-node [[child-node-count metadata-entry-count & more]]
  (if (zero? child-node-count) {:child-node-count child-node-count
                                :metadata-entry-count metadata-entry-count
                                :child-nodes nil
                                :metadata-entries (take metadata-entry-count more)
                                :node-size (+ 2 child-node-count metadata-entry-count)}
      (let [child-nodes (get-child-nodes more child-node-count)
            child-node-length (reduce + (map :node-size child-nodes))]
        {:child-node-count child-node-count
         :metadata-entry-count metadata-entry-count
         :child-nodes child-nodes
         :metadata-entries (take metadata-entry-count (drop child-node-length more))
         :node-size (+ 2 child-node-length metadata-entry-count)})))

(defn collect-metadata-entries
  [node]
  (if-not (:child-nodes node) (:metadata-entries node)
          (concat (:metadata-entries node) (map collect-metadata-entries (:child-nodes node)))))

(defn part-1 [input]
  (let [instructions (parse-instructions input)
        root-node (parse-node instructions)
        metadata (flatten (collect-metadata-entries root-node))]
    (reduce + metadata)))

(defn get-node-value [node]
  (let [child-nodes (:child-nodes node)
        metadata-entries (:metadata-entries node)]
    (if-not child-nodes (reduce + metadata-entries)
            (let [child-node-values (map (fn [metadata-entry]
                                           (let [metadata-index (dec metadata-entry)
                                                 valid-index? (and (> metadata-entry 0) (<= metadata-index (count child-nodes)))]
                                             (if valid-index? (get-node-value (get child-nodes metadata-index)) 0))) metadata-entries)]
              (reduce + child-node-values)))))

(defn part-2 [input]
  (let [instructions (parse-instructions input)
        root-node  (parse-node instructions)
        node-value (get-node-value root-node)]
    node-value))

(comment
  (= 41849 (part-1 input))
  (= 32487 (part-2 input)))
