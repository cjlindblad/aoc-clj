(ns aoc-clj.2019.07.solution
  (:require [clojure.string :as s]))

(defn parse-int [n] (Integer/parseInt n))

(def input (slurp "src/aoc_clj/2019/07/input.txt"))

(defn parse-memory [input]
  (mapv (comp parse-int s/trim) (s/split input #",")))

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
      "99" {:memory memory :address address :input input :output output :halted true}
      "01" (let [args (instruction-args memory (instruction->modes instruction 2) address)]
             (recur
              (assoc memory (get memory (+ address 3)) (reduce + args))
              (+ 4 address)
              input
              output))
      "02" (let [args (instruction-args memory (instruction->modes instruction 2) address)]
             (recur
              (assoc memory
                     (get memory
                          (+ address 3))
                     (reduce * args))
              (+ 4 address)
              input
              output))
      "03" (let [current-input (first input)]
             (if-not current-input {:memory memory :address address :input input :output output :halted false}
                     (recur
                      (assoc memory (get memory (+ address 1)) current-input)
                      (+ 2 address)
                      (rest input)
                      output)))
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

(defn settings-variants [min max]
  (for [a (range min (inc max))
        b (range min (inc max))
        c (range min (inc max))
        d (range min (inc max))
        e (range min (inc max))
        :when (= (count [a b c d e]) (count (apply hash-set [a b c d e])))]
    [a b c d e]))

(defn run-amplifiers [memory [a b c d e]]
  (let [output-a (:output (execute memory 0 [a 0] nil))
        output-b (:output (execute memory 0 [b output-a] nil))
        output-c (:output (execute memory 0 [c output-b] nil))
        output-d (:output (execute memory 0 [d output-c] nil))
        output-e (:output (execute memory 0 [e output-d] nil))]
    output-e))

(defn part-1 [input]
  (let [memory (parse-memory input)
        thruster-signals (map (fn [settings] (run-amplifiers memory settings)) (settings-variants 0 4))]
    (apply max thruster-signals)))

(defn run-amplifiers-2 [memory [a b c d e]]
  (let [state-a (execute memory 0 [a 0] nil)
        state-b (execute memory 0 [b (:output state-a)] nil)
        state-c (execute memory 0 [c (:output state-b)] nil)
        state-d (execute memory 0 [d (:output state-c)] nil)
        state-e (execute memory 0 [e (:output state-d)] nil)]
    (loop [s-a state-a
           s-b state-b
           s-c state-c
           s-d state-d
           s-e state-e]
      (if (:halted s-e) (:output s-e)
          (let [next-state-a (execute (:memory s-a) (:address s-a) [(:output s-e)] nil)
                next-state-b (execute (:memory s-b) (:address s-b) [(:output next-state-a)] nil)
                next-state-c (execute (:memory s-c) (:address s-c) [(:output next-state-b)] nil)
                next-state-d (execute (:memory s-d) (:address s-d) [(:output next-state-c)] nil)
                next-state-e (execute (:memory s-e) (:address s-e) [(:output next-state-d)] nil)]
            (recur next-state-a next-state-b next-state-c next-state-d next-state-e))))))

(defn part-2 [input]
  (let [memory (parse-memory input)
        thruster-signals (map (fn [settings] (run-amplifiers-2 memory settings)) (settings-variants 5 9))]
    (apply max thruster-signals)))

(comment
  (= 75228 (part-1 input))
  (= 79846026 (part-2 input)))

