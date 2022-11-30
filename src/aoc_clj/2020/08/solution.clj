(ns aoc-clj.2020.08.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2020/08/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2020/08/test-input.txt")))

(defn parse-instruction [line]
  (let [[instruction value] (string/split line #" ")
        int-value (Integer/parseInt value)]
    (case instruction
      "nop" {:instruction :nop :value int-value}
      "acc" {:instruction :acc :value int-value}
      "jmp" {:instruction :jmp :value int-value})))

(defn starting-state []
  {:accumulator 0 :instruction-pointer 0})

(defn execute-instruction [{:keys [accumulator instruction-pointer] :as state} {:keys [instruction value]}]
  (case instruction
    :nop (assoc state :instruction-pointer (inc instruction-pointer))
    :acc (-> state
             (assoc :accumulator (+ accumulator value))
             (assoc :instruction-pointer (inc instruction-pointer)))
    :jmp (assoc state :instruction-pointer (+ instruction-pointer value))))

(defn execute-program
  ([state instructions] (execute-program state instructions #{}))
  ([{:keys [instruction-pointer] :as state} instructions executed-instruction-pointers]
   (if (= instruction-pointer (count instructions)) (assoc state :finished true)
       (if (executed-instruction-pointers instruction-pointer) (assoc state :finished false)
           (let [next-state (execute-instruction state (nth instructions instruction-pointer))]
             (recur next-state instructions (conj executed-instruction-pointers (:instruction-pointer state))))))))

(defn part-1 [input]
  (let [instructions (mapv parse-instruction input)
        state (starting-state)
        final-state (execute-program state instructions)]
    (:accumulator final-state)))

(defn instruction-variants
  ([instructions] (instruction-variants instructions 0 []))
  ([instructions index results]
   (if (>= index (count instructions)) results
       (let [{:keys [instruction value]} (nth instructions index)
             next-instruction {:instruction (get {:nop :jmp, :jmp :nop} instruction instruction) :value value}
             next-instructions (concat (take index instructions) [next-instruction] (drop (inc index) instructions))]
         (recur instructions (inc index) (distinct (conj results next-instructions)))))))

(defn part-2 [input]
  (let [instructions (mapv parse-instruction input)
        variants (instruction-variants instructions)
        state (starting-state)
        states (map (fn [instrs] (execute-program state instrs)) variants)]
    (:accumulator (first (filter :finished states)))))

(comment
  (= 1217 (part-1 input))
  (= 501 (part-2 input)))
