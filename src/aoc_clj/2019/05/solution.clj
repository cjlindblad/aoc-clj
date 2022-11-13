(ns aoc-clj.2019.05.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (mapv (comp parse-int s/trim) (s/split (slurp "src/aoc_clj/2019/05/input.txt") #",")))

(def modes
  {\0 :position
   \1 :immediate})

(defn instruction->modes [instruction param-count]
  (as-> (str instruction) $
    (drop-last 2 $)
    (reverse $)
    (concat $ (repeat \0))
    (take param-count $)
    (map modes $)))

(defn get-memory [memory address mode]
  (case mode
    :position (get memory (get memory address))
    :immediate (get memory address)))

(defn instruction-args [memory modes address]
  (->> (map-indexed (fn [i mode] [(inc i) mode]) modes)
       (map (fn [[offset mode]] (get-memory memory (+ address offset) mode)))))

(defn parse-opcode [instruction]
  (as-> (str instruction) $
    (take-last 2 $)
    (conj $ \0)
    (take-last 2 $)
    (apply str $)))

(defn execute [memory address input output]
  (let [instruction (get memory address)
        opcode (parse-opcode instruction)]
    (case opcode
      "99" output
      "01" (let [args (instruction-args memory (instruction->modes instruction 2) address)]
             (recur
              (assoc memory (get memory (+ address 3)) (reduce + args))
              (+ 4 address)
              input
              output))
      "02" (let [args (instruction-args memory (instruction->modes instruction 2) address)]
             (recur
              (assoc memory (get memory (+ address 3)) (reduce * args))
              (+ 4 address)
              input
              output))
      "03" (recur
            (assoc memory (get memory (+ address 1)) input)
            (+ 2 address)
            input
            output)
      "04" (recur
            memory
            (+ 2 address)
            input
            (get-memory memory (+ address 1) :position))
      "05" (let [args (instruction-args memory (instruction->modes instruction 2) address)
                 predicate (first args)
                 jump? (not= 0 predicate)
                 destination (second args)]
             (recur
              memory
              (if jump? destination (+ 3 address))
              input
              output))
      "06" (let [args (instruction-args memory (instruction->modes instruction 2) address)
                 predicate (first args)
                 jump? (= 0 predicate)
                 destination (second args)]
             (recur
              memory
              (if jump? destination (+ 3 address))
              input
              output))
      "07" (let [args (instruction-args memory (instruction->modes instruction 2) address)
            position (get memory (+ address 3))
            result (if-not (< (first args) (second args)) 0 1)]
        (recur
         (assoc memory position result)
         (+ 4 address)
         input
         output))
      "08" (let [args (instruction-args memory (instruction->modes instruction 2) address)
            position (get memory (+ address 3))
            result (if-not (= (first args) (second args)) 0 1)]
        (recur
         (assoc memory position result)
         (+ 4 address)
         input
         output)))))

(defn part-1 [input]
  (execute input 0 1 nil))

(defn part-2 [input]
  (execute input 0 5 nil))

(comment
  (= 7566643 (part-1 input))
  (= 9265694 (part-2 input)))
