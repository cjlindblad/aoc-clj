(ns aoc-clj.2021.04.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(defn parse-int [n] (Integer/parseInt n))

(def test-input (s/split (slurp "src/aoc_clj/2021/04/test-input.txt") #"\n\n"))
(def input (s/split (slurp "src/aoc_clj/2021/04/input.txt") #"\n\n"))

(def board-input (second test-input))

(defn parse-board [input]
  (let [numbers (s/split input #"\s+")]
    {:numbers (map parse-int numbers)
     :marked #{}}))

(def board (parse-board board-input))

(defn board-has-number? [board n]
  ((apply hash-set (:numbers board)) n))

(defn mark-number [board n]
  (if-not (board-has-number? board n) board
          (update board :marked #(conj % n))))

(defn board-cols [board]
  (partition 5 (:numbers board)))

(defn board-rows [board]
  (apply map vector (board-cols board)))

(defn line-marked? [line marked]
  (reduce (fn [acc cur] (and acc (marked cur))) true line))

(defn unmarked-numbers [board]
  (let [numbers-set (apply hash-set (:numbers board))]
    (set/difference numbers-set (:marked board))))

(defn bingo? [board]
  (let [cols (board-cols board)
        rows (board-rows board)
        lines (concat cols rows)]
    (some #(line-marked? % (:marked board)) lines)))

(defn first-winning-board [[current-number & rest-numbers] boards]
  (let [next-boards (map #(mark-number % current-number) boards)
        winning-board (first (filter bingo? next-boards))]
    (if winning-board [winning-board current-number]
        (recur rest-numbers next-boards))))

(defn last-winning-board [[current-number & rest-numbers] boards]
  (let [next-boards (map #(mark-number % current-number) boards)
        winning-board (first (filter bingo? next-boards))]
    (if (and (= 1 (count boards)) winning-board) [winning-board current-number]
        (recur rest-numbers (filter #(not (bingo? %)) next-boards)))))

(defn part-1 [input]
  (let [numbers (map parse-int (s/split (first input) #","))
        boards (map parse-board (map s/trim (rest input)))
        [winning-board winning-number] (first-winning-board numbers boards)
        unmarked-sum (apply + (unmarked-numbers winning-board))]
    (* unmarked-sum winning-number)))

(defn part-2 [input]
  (let [numbers (map parse-int (s/split (first input) #","))
        boards (map parse-board (map s/trim (rest input)))
        [winning-board winning-number] (last-winning-board numbers boards)
        unmarked-sum (apply + (unmarked-numbers winning-board))]
    (* unmarked-sum winning-number)))

(comment
  (= 44088 (part-1 input))
  (= 23670 (part-2 input)))


