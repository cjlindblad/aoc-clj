(ns aoc-clj.2016.05.solution
  (:require [aoc-clj.utils :as utils]))

(def input "wtnhxymk")
(def test-input "abc")

(defn find-password
  ([input] (find-password input 0 []))
  ([input index pw-chars]
   (if (= 8 (count pw-chars)) (apply str pw-chars)
       (let [hash (utils/md5 (str input index))
             matching-hash (re-seq #"^00000" hash)]
         (if-not matching-hash (recur input (inc index) pw-chars)
                 (let [pw-char (first (drop 5 hash))]
                   (recur input (inc index) (conj pw-chars pw-char))))))))

(defn part-1 [input]
  (find-password input))

(defn find-password-2
  ([input] (find-password-2 input 0 (sorted-map)))
  ([input index pw-chars]
   (if (= 8 (count pw-chars)) (apply str (vals pw-chars))
       (let [hash (utils/md5 (str input index))
             matching-hash (re-seq #"^00000[0-7]" hash)]
         (if-not matching-hash (recur input (inc index) pw-chars)
                 (let [pw-index (Integer/parseInt (str (first (drop 5 hash))))
                       pw-char (first (drop 6 hash))
                       next-pw-chars (if (get pw-chars pw-index) pw-chars (assoc pw-chars pw-index pw-char))]
                   (recur input (inc index) next-pw-chars)))))))

(defn part-2 [input]
  (find-password-2 input))

(-> (sorted-map)
    (assoc 3 "z")
    (into {2 "y"})
    (into {1 "x"})
    vals)

(comment
  (= "2414bc77" (part-1 input))
  (= "437e60fc" (part-2 input))
  )
