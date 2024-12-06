(ns aoc-clj.utils)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defmacro bench
  ([& forms]
   `(let [start# (System/currentTimeMillis)]
      ~@forms
      (- (System/currentTimeMillis) start#))))

(defn first-duplicate
  ([xs] (first-duplicate xs #{}))
  ([xs seen]
   (if (empty? xs) nil
       (let [x (first xs)]
         (if (seen x) x
             (recur (rest xs) (conj seen x)))))))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn take-upto
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-upto pred (rest s)))))))

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))
