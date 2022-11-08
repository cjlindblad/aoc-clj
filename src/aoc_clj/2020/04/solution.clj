(ns aoc-clj.2020.04.solution
  (:require [clojure.string :as s]
            [clojure.set :as set]))

(def input (s/split (slurp "src/aoc_clj/2020/04/input.txt") #"\n\n"))
(def test-input (s/split (slurp "src/aoc_clj/2020/04/test-input.txt") #"\n\n"))

(defn parse-entry [entry]
  (let [[k v] (s/split entry #":")]
    [(keyword k) v]))

(defn parse-passport [line]
  (->> (s/split line #"\s+")
       (map parse-entry)
       (into {})))

(defn required-fields? [passport]
  (let [existing-keys (apply hash-set (keys passport))]
    (set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} existing-keys)))

(defn byr-valid? [byr]
  (<= 1920 (Integer/parseInt byr) 2002))

(defn iyr-valid? [iyr]
  (<= 2010 (Integer/parseInt iyr) 2020))

(defn eyr-valid? [eyr]
  (<= 2020 (Integer/parseInt eyr) 2030))

(defn hgt-valid? [hgt]
  (let [value (Integer/parseInt (first (re-seq #"\d+" hgt)))]
    (cond (re-seq #"cm" hgt) (<= 150 value 193)
          (re-seq #"in" hgt) (<= 59 value 76)
          :else false)))

(defn hcl-valid? [hcl]
  (re-seq #"^#[a-f0-9]{6}$" hcl))

(defn ecl-valid? [ecl]
  (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl))

(defn pid-valid? [pid]
  (re-seq #"^\d{9}$" pid))

(defn valid-passport? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and
   (byr-valid? byr)
   (iyr-valid? iyr)
   (eyr-valid? eyr)
   (hgt-valid? hgt)
   (hcl-valid? hcl)
   (ecl-valid? ecl)
   (pid-valid? pid)))

(defn part-1 [input]
  (->> input
       (map parse-passport)
       (filter required-fields?)
       count))

(defn part-2 [input]
  (->> input
       (map parse-passport)
       (filter required-fields?)
       (filter valid-passport?)
       count))

(comment
  (= 239 (part-1 input))
  (= 188 (part-2 input)))
