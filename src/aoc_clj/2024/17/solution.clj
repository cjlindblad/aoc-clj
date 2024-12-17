(ns aoc-clj.2024.17.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/17/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/17/test-input.txt"))

(defn parse-long [n] (Long/parseLong n))

(defn parse-input [input]
  (let [[_ a b c instr-str]
        (->>  (str/replace input "\n" "")
              (re-seq #"(?s)A: (\d+).*B: (\d+).*C: (\d+).*Program: (.+)")
              first)]
    {:reg-a (parse-long a)
     :reg-b (parse-long b)
     :reg-c (parse-long c)
     :instructions (mapv parse-long (str/split instr-str #","))}))

(defn get-combo [state operand]
  (case operand
    0 0
    1 1
    2 2
    3 3
    4 (@state :A)
    5 (@state :B)
    6 (@state :C)
    7 (throw (Exception. "got combo operand 7"))))

(defn execute [{:keys [reg-a reg-b reg-c instructions]}]
  (let [state (atom {:A reg-a
                     :B reg-b
                     :C reg-c
                     :out []})]
    (loop [ip 0]
      (if (<= (count instructions) ip)
        @state
        (let [instruction (instructions ip)
              operand (instructions (inc ip))]
          (case
           instruction
            0 (let [numerator (@state :A)
                    combo (get-combo state operand)
                    denominator (int (Math/pow 2 combo))
                    result (quot numerator denominator)]
                (swap! state assoc :A result)
                (recur (+ ip 2)))

            1 (let [left (@state :B)
                    right operand
                    result (bit-xor left right)]
                (swap! state assoc :B result)
                (recur (+ ip 2)))

            2 (let [combo (get-combo state operand)
                    result (mod combo 8)]
                (swap! state assoc :B result)
                (recur (+ ip 2)))

            3 (let [A (@state :A)]

                (if (zero? A)
                  (recur (+ ip 2))
                  (recur operand)))

            4 (let [left (@state :B)
                    right (@state :C)
                    result (bit-xor left right)]
                (swap! state assoc :B result)
                (recur (+ ip 2)))

            5 (let [combo (get-combo state operand)
                    result (mod combo 8)
                    next-out (conj (@state :out) result)]
                (swap! state assoc :out next-out)
                (recur (+ ip 2)))

            6 (let [numerator (@state :A)
                    combo (get-combo state operand)
                    denominator (int (Math/pow 2 combo))
                    result (quot numerator denominator)]
                (swap! state assoc :B result)
                (recur (+ ip 2)))
            7 (let [numerator (@state :A)
                    combo (get-combo state operand)
                    denominator (int (Math/pow 2 combo))
                    result (quot numerator denominator)]
                (swap! state assoc :C result)
                (recur (+ ip 2)))))))))

(defn part-1 [input]
  (let [parsed-input (parse-input input)
        final-state (execute parsed-input)]
    (->> (:out final-state)
         (str/join ","))))

(comment
  (= "2,1,0,4,6,2,4,2,0" (part-1 input)))
