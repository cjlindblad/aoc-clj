(ns aoc-clj.2015.04.solution)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(def input "ckczppom")

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn find-suffix
  ([input regex] (find-suffix input regex 1))
  ([input regex n]
   (let [hash (md5 (str input n))]
     (if (re-seq regex hash) n
         (recur input regex (inc n))))))

(defn part-1 [input]
  (find-suffix input #"^00000"))

(defn part-2 [input]
  (find-suffix input #"^000000"))

(comment
  (= 117946 (part-1 input))
  (= 3938038 (part-2 input)))

