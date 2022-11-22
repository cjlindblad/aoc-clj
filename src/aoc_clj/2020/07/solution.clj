(ns aoc-clj.2020.07.solution
  (:require [clojure.string :as s]))

(def input (s/split-lines (slurp "src/aoc_clj/2020/07/input.txt")))
(def test-input (s/split-lines (slurp "src/aoc_clj/2020/07/test-input.txt")))

(defn parse-sub-bag [sub-bag-string]
  (if (re-seq #"no other bags" sub-bag-string) nil
      (let [[amount variant color] (s/split sub-bag-string #" ")]
        {:amount (Integer/parseInt amount)
         :bag (str variant " " color)})))

(defn parse-rule [line]
  (let [[main-bag sub-bags-string] (s/split line #" bags contain ")
        sub-bags (s/split sub-bags-string #", ")]
    {:main-bag main-bag
     :sub-bags (map parse-sub-bag sub-bags)}))

(defn rules->rule-graph
  ([rules] (rules->rule-graph rules {}))
  ([rules graph]
   (if-not (seq rules) graph
           (let [{:keys [main-bag sub-bags]} (first rules)]
             (recur (rest rules) (assoc graph main-bag sub-bags))))))

(defn contains-shiny-gold? [bag graph]
  (cond
    (nil? bag) false
    (= "shiny gold" bag) true
    :else (let [sub-bags (map :bag (graph bag))
                sub-bags-result (map (fn [sub-bag] (contains-shiny-gold? sub-bag graph)) sub-bags)]
            (some identity sub-bags-result))))

(defn total-bags [bag graph]
  (let [sub-bags (graph bag)
        sub-bag-count (map (fn [sub-bag]
                             (if (nil? sub-bag) 0
                                 (let [{:keys [amount bag]} sub-bag]
                                   (+ amount (* amount (total-bags bag graph)))))) sub-bags)]
    (reduce + sub-bag-count)))

(defn part-1 [input]
  (let [rules (map parse-rule input)
        graph (rules->rule-graph rules)
        bags (keys graph)
        has-shiny-gold (filter (fn [bag] (and (not= "shiny gold" bag) (contains-shiny-gold? bag graph))) bags)]
    (count has-shiny-gold)))

(defn part-2 [input]
  (let [rules (map parse-rule input)
        graph (rules->rule-graph rules)
        bags (total-bags "shiny gold" graph)]
    bags))

(comment
  (= 226 (part-1 input))
  (= 9569 (part-2 input)))

