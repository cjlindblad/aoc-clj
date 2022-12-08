(ns aoc-clj.2022.07.solution
  (:require [clojure.string :as string]))

(def input (string/split-lines (slurp "src/aoc_clj/2022/07/input.txt")))
(def test-input (string/split-lines (slurp "src/aoc_clj/2022/07/test-input.txt")))

(defn parse-line [line]
  (let [[[_ cd-arg]] (re-seq #"^\$ cd (.+)" line)
        ls (re-seq #"^\$ ls$" line)
        [[_ file-size file-name]] (re-seq #"^(\d+) (.+)" line)
        [[_ dir-name]] (re-seq #"^dir (.+)" line)]
    (cond cd-arg {:type :cd :arg cd-arg}
          ls {:type :ls}
          file-name {:type :file :size (Long/parseLong file-size) :name file-name}
          dir-name {:type :dir :name dir-name})))

(defn cd? [entry] (= :cd (:type entry)))
(defn ls? [entry] (= :ls (:type entry)))
(defn dir? [entry] (= :dir (:type entry)))
(defn file? [entry] (= :file (:type entry)))

(defn dir-with-absolut-name [cwd dir]
  (->> (:name dir)
       (str cwd "/")
       (assoc dir :name)))

(defn build-tree
  ([lines] (build-tree lines [] {}))
  ([lines dir-stack tree]
   (if (seq lines)
     (let [ls-entries (take-while (complement cd?) lines)]
       (if-not (seq ls-entries)
         (let [cd-arg (:arg (first lines))]
           (case cd-arg
             ".." (recur (rest lines) (drop-last dir-stack) tree)
             (recur (rest lines) (concat dir-stack [cd-arg]) tree)))
         (let [cwd (->> (interpose "/" dir-stack)
                        (apply str))
               files (filter file? ls-entries)
               dirs (->> (filter dir? ls-entries)
                         (map (partial dir-with-absolut-name cwd)))
               next-node {cwd {:files files :dirs dirs}}]
           (recur (drop (count ls-entries) lines) dir-stack (merge tree next-node)))))
     tree)))

(defn dir-size [tree dir]
  (let [{:keys [files dirs]} (tree dir)]
    (+ (reduce + (map :size files))
       (reduce + (map (partial dir-size tree) (map :name dirs))))))

(defn input->tree [input]
  (->> (map parse-line input)
       (filter (complement ls?))
       build-tree))

(defn dir-sizes [tree]
  (->> (keys tree)
       (map (partial dir-size tree))))

(defn part-1 [input]
  (let [tree (input->tree input)
        sizes (dir-sizes tree)
        small (filter #(<= % 100000) sizes)]
    (apply + small)))

(defn part-2 [input]
  (let [tree (input->tree input)
        sizes (dir-sizes tree)
        min-delete-size (- 30000000 (- 70000000 (apply max sizes)))
        candidates (filter (partial <= min-delete-size) sizes)]
    (apply min candidates)))

(comment
  (= 1989474 (part-1 input))
  (= 1111607 (part-2 input)))
