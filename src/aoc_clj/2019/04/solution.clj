(ns aoc-clj.2019.04.solution)

(defn number-to-seq
  ([n] (number-to-seq n []))
  ([n nums]
   (if (< n 10) (reverse (conj nums n))
       (recur (quot n 10) (conj nums (mod n 10))))))

(defn non-decreasing? [ns]
  (apply <= ns))

(defn adjacent-double? [ns]
  (let [pairs (partition 2 1 ns)]
    (some #(apply = %) pairs)))

(defn exact-double? [ns]
  (let [groups (partition-by identity ns)]
    (some #(= 2 (count %)) groups)))

(defn valid-password? [n]
  (let [n-seq (number-to-seq n)]
    (and (non-decreasing? n-seq)
         (adjacent-double? n-seq))))

(defn valid-password-2? [n]
  (let [n-seq (number-to-seq n)]
    (and (non-decreasing? n-seq)
         (exact-double? n-seq))))

(defn part-1 [min max]
  (let [ns (range min (inc max))]
    (-> (filter valid-password? ns)
        count)))

(defn part-2 [min max]
  (let [ns (range min (inc max))]
    (-> (filter valid-password-2? ns)
        count)))

(comment
  (= 921 (part-1 278384 824795))
  (= 603 (part-2 278384 824795)))
