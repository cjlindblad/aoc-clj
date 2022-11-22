(ns aoc-clj.2015.07.solution
  (:require [clojure.string :as s]))

(defn parse-int [n]
  (if (number? n) n
      (Integer/parseInt n)))

(def input (s/split-lines (slurp "src/aoc_clj/2015/07/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2015/07/test-input.txt")))

(defn string-or-int [x]
  (if (re-seq #"^\d+$" x) (Integer/parseInt x)
      x))

(defn parse-lval [raw]
  (let [words (s/split raw #"\s+")
        param-1 (string-or-int (first words))
        param-2 (string-or-int (last words))]
    (cond (re-seq #"^\d+$" raw) (Integer/parseInt raw)
          (re-seq #"AND" raw) {:op :AND :param-1 param-1 :param-2 param-2}
          (re-seq #"OR" raw) {:op :OR :param-1 param-1 :param-2 param-2}
          (re-seq #"LSHIFT" raw) {:op :LSHIFT :param-1 param-1 :param-2 param-2}
          (re-seq #"RSHIFT" raw) {:op :RSHIFT :param-1 param-1 :param-2 param-2}
          (re-seq #"NOT" raw) {:op :NOT :param-1 param-2}
          :else raw)))

(defn parse-connection [line]
  (let [[from to] (s/split line #" -> ")]
    [(parse-lval from) to]))

(defn build-graph
  ([connections] (build-graph connections {}))
  ([connections graph]
   (if-not (seq connections) graph
           (let [[from to] (first connections)]
             (recur (rest connections) (assoc graph to from))))))

(defn value-for
  ([graph wire] (value-for graph {} 0 (keys graph) wire))
  ([graph wire result] (value-for graph result 0 (keys graph) wire))
  ([graph result index wires wire]
   (if (result wire) (result wire)
       (let [current-wire (nth wires (mod index (count wires)))
             current-instruction (graph current-wire)]
         (if (and (not (result current-wire)) (number? current-instruction)) (recur graph (assoc result current-wire current-instruction) (inc index) wires wire)
             (if (and (string? current-instruction) (result current-instruction)) (recur graph (assoc result current-wire (result current-instruction)) (inc index) wires wire)
                 (if (:op current-instruction)
                   (let [{:keys [op param-1 param-2]} current-instruction
                         param-1-val (if (number? param-1) param-1 (result param-1))
                         param-2-val (if (number? param-2) param-2 (result param-2))]
                     (case op
                       :AND (if-not (and param-1-val param-2-val)
                              (recur graph result (inc index) wires wire)
                              (recur graph (assoc result current-wire (bit-and param-1-val param-2-val)) (inc index) wires wire))
                       :OR (if-not (and param-1-val param-2-val)
                             (recur graph result (inc index) wires wire)
                             (recur graph (assoc result current-wire (bit-or param-1-val param-2-val)) (inc index) wires wire))
                       :LSHIFT (if-not (and param-1-val param-2-val)
                                 (recur graph result (inc index) wires wire)
                                 (recur graph (assoc result current-wire (bit-shift-left param-1-val param-2-val)) (inc index) wires wire))
                       :RSHIFT (if-not (and param-1-val param-2-val)
                                 (recur graph result (inc index) wires wire)
                                 (recur graph (assoc result current-wire (unsigned-bit-shift-right param-1-val param-2-val)) (inc index) wires wire))
                       :NOT (if-not param-1-val
                              (recur graph result (inc index) wires wire)
                              (recur graph (assoc result current-wire (bit-and-not 16rFFFF param-1-val)) (inc index) wires wire))))
                   (recur graph result (inc index) wires wire))))))))

(defn part-1 [input]
  (let [graph (->> (map parse-connection input)
                   build-graph)]
    (value-for graph "a")))

(defn part-2 [input]
  (let [graph (->> (map parse-connection input)
                   build-graph)]
    (value-for graph "a" {"b" (part-1 input)})))

(comment
  (= 46065 (part-1 input))
  (= 14134 (part-2 input)))
