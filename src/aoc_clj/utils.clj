(ns aoc-clj.utils)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

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

