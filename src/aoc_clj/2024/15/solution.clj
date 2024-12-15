(ns aoc-clj.2024.15.solution
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc_clj/2024/15/input.txt"))
(def test-input (slurp "src/aoc_clj/2024/15/test-input.txt"))
(def small-test-input (slurp "src/aoc_clj/2024/15/small-test-input.txt"))

(defn parse-map [lines [max-x max-y]]
  (let [positions
        (->> (for [x (range (inc max-x)) y (range (inc max-y))]
               [(get-in lines [y x]) [x y]])
             (group-by first)
             (map (fn [[k v]] [k (into #{} (map last v))]))
             (into {}))]
    {:walls (positions \#)
     :boxes (positions \O)
     :robot (positions \@)}))

(defn parse-moves [moves-string]
  (as-> (str/replace moves-string "\n" "") %
    (str/split % #"")
    (map str %)))

(defn parse-input [input]
  (let [[map-string moves-string] (str/split input #"\n\n")
        map-lines (str/split map-string #"\n")
        max-y (dec (count map-lines))
        max-x (dec (count (first map-lines)))]
    {:warehouse (parse-map map-lines [max-x max-y])
     :max-x max-x
     :max-y max-y
     :directions (parse-moves moves-string)}))

(defn at-warehouse-pos [warehouse pos]
  (cond
    ((warehouse :walls) pos) "#"
    ((warehouse :boxes) pos) "O"
    ((warehouse :robot) pos) "@"
    :else "."))

(defn visualize [warehouse [max-x max-y]]
  (->> (for [y (range (inc max-y))]
         (for [x (range (inc max-x))]
           (let [pos [x y]]
             (at-warehouse-pos warehouse pos))))
       (map #(str/join "" %))))

(defn direction-delta [direction]
  (case direction
    "^" [0 -1]
    ">" [1 0]
    "v" [0 1]
    "<" [-1 0]))

(defn apply-delta [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn get-pushable-line [warehouse pos delta]
  (loop [line [] pos pos]
    (let [next-pos (apply-delta pos delta)
          next-thing (at-warehouse-pos warehouse next-pos)]
      (case next-thing
        "#" nil
        "." (conj line next-pos)
        "O" (recur (conj line next-pos) next-pos)))))

(defn move [warehouse direction]
  (let [delta (direction-delta direction)
        robot (first (:robot warehouse))
        next-robot-pos (apply-delta robot delta)
        next-location (at-warehouse-pos warehouse next-robot-pos)]
    (case next-location
      "#" warehouse
      "." (assoc warehouse :robot #{next-robot-pos})
      "O" (let [pushable-line (get-pushable-line warehouse next-robot-pos delta)]
            (if-not pushable-line
              warehouse
              (let [boxes (:boxes warehouse)
                    next-boxes (-> (into boxes pushable-line)
                                   (disj boxes next-robot-pos))]
                (-> (assoc warehouse :robot #{next-robot-pos})
                    (assoc :boxes next-boxes))))))))

(defn score [warehouse]
  (->> (:boxes warehouse)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

(defn part-1 [input]
  (let [{:keys [warehouse directions]} (parse-input input)
        final-warehouse (reduce (fn [w d] (move w d)) warehouse directions)]
    (score final-warehouse)))

(defn widened-pair [thing]
  (case thing
    \. [\. \.]
    \@ [\@ \.]
    \# [\# \#]
    \O [\[ \]]
    nil))

(defn parse-map-2 [lines [max-x max-y]]
  (let [positions
        (->> (for [x (range (inc max-x)) y (range (inc max-y))]
               (let [[left right] (widened-pair (get-in lines [y x]))]
                 [[left [(* 2 x) y]]
                  [right [(inc (* 2 x)) y]]]))
             (mapcat identity)
             (group-by first)
             (map (fn [[k v]] [k (into #{} (map last v))]))
             (into {}))]
    {:walls (positions \#)
     :left-boxes (positions \[)
     :right-boxes (positions \])
     :robot (positions \@)}))

(defn parse-input-2 [input]
  (let [[map-string moves-string] (str/split input #"\n\n")
        map-lines (str/split map-string #"\n")
        max-y (dec (count map-lines))
        max-x (dec (* 2 (count (first map-lines))))]
    {:warehouse (parse-map-2 map-lines [max-x max-y])
     :max-x max-x
     :max-y max-y
     :directions (parse-moves moves-string)}))

(defn at-warehouse-pos-2 [warehouse pos]
  (cond
    ((warehouse :walls) pos) "#"
    ((warehouse :left-boxes) pos) "["
    ((warehouse :right-boxes) pos) "]"
    ((warehouse :robot) pos) "@"
    :else "."))

(defn visualize-2 [warehouse [max-x max-y]]
  (->> (for [y (range (inc max-y))]
         (for [x (range (inc max-x))]
           (let [pos [x y]]
             (at-warehouse-pos-2 warehouse pos))))
       (map #(str/join "" %))))

(defn get-pushable-line-2 [warehouse pos delta]
  (loop [line [] pos pos]
    (let [next-pos (apply-delta pos delta)
          next-thing (at-warehouse-pos-2 warehouse pos)]
      (case next-thing
        "#" nil
        "." line
        "[" (recur (conj line [:left-boxes pos next-pos]) next-pos)
        "]" (recur (conj line [:right-boxes pos next-pos]) next-pos)))))

(defn update-warehouse [warehouse updates]
  (reduce
   (fn [w [thing old new]]
     (-> (update w thing disj old)
         (update thing conj new)))
   warehouse updates))

(defn shift-left [[x y]] [(dec x) y])
(defn shift-right [[x y]] [(inc x) y])

(defn vertical-box-push [warehouse left-pos right-pos delta]
  (let [next-left-pos (apply-delta left-pos delta)
        next-left (at-warehouse-pos-2 warehouse next-left-pos)
        next-right-pos (apply-delta right-pos delta)
        next-right (at-warehouse-pos-2 warehouse next-right-pos)
        this-positions [[:left-boxes left-pos next-left-pos]
                        [:right-boxes right-pos next-right-pos]]]
    (cond
      (or (= "#" next-left) (= "#" next-right))
      nil

      (= ["." "."] [next-left next-right])
      this-positions

      (= ["[" "]"] [next-left next-right])
      (let [next-push (vertical-box-push warehouse next-left-pos next-right-pos delta)]
        (when (seq next-push)
          (concat next-push this-positions)))

      (= ["]" "["] [next-left next-right])
      (let [next-left-push (vertical-box-push warehouse (shift-left next-left-pos) next-left-pos delta)
            next-right-push (vertical-box-push warehouse next-right-pos (shift-right next-right-pos) delta)]
        (when (and (seq next-left-push) (seq next-right-push))
          (concat (concat next-left-push next-right-push) this-positions)))

      (= "]" next-left)
      (let [next-push (vertical-box-push warehouse (shift-left next-left-pos) next-left-pos delta)]
        (when (seq next-push)
          (concat next-push this-positions)))

      (= "[" next-right)
      (let [next-push (vertical-box-push warehouse next-right-pos (shift-right next-right-pos) delta)]
        (when (seq next-push)
          (concat next-push this-positions))))))

(defn move-2 [warehouse direction]
  (let [delta (direction-delta direction)
        robot (first (:robot warehouse))
        next-robot-pos (apply-delta robot delta)
        next-location (at-warehouse-pos-2 warehouse next-robot-pos)
        get-horizontal-positions (fn [] (get-pushable-line-2 warehouse next-robot-pos delta))
        get-vertical-positions (fn [left right] (vertical-box-push warehouse left right delta))]
    (case next-location
      "#" warehouse
      "." (assoc warehouse :robot #{next-robot-pos})
      (let [positions-to-update
            (case next-location
              "[" (case direction
                    ">" (get-horizontal-positions)
                    "^" (get-vertical-positions next-robot-pos (shift-right next-robot-pos))
                    "v" (get-vertical-positions next-robot-pos (shift-right next-robot-pos)))
              "]" (case direction
                    "<" (get-horizontal-positions)
                    "^" (get-vertical-positions (shift-left next-robot-pos) next-robot-pos)
                    "v" (get-vertical-positions (shift-left next-robot-pos) next-robot-pos)))]
        (update-warehouse warehouse (if (seq positions-to-update) (conj positions-to-update [:robot robot next-robot-pos]) positions-to-update))))))

(defn score-2 [warehouse]
  (->> (:left-boxes warehouse)
       (map (fn [[x y]] (+ x (* 100 y))))
       (reduce +)))

(defn part-2 [input]
  (let [{:keys [warehouse directions]} (parse-input-2 input)
        final-warehouse (reduce (fn [w d] (move-2 w d)) warehouse directions)]
    (score-2 final-warehouse)))

(comment
  (= 1398947 (part-1 input))
  (= 1397393 (part-2 input)))

