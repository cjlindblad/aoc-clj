(ns aoc-clj.2017.03.solution)

(def input 265149)

(defn border-width [n]
  (-> (for [x (iterate #(+ 2 %) 1)
            :while (< (* x x) n)]
        x)
      count
      inc
      (take (iterate #(+ 2 %) 1))
      last))

(defn width->coordinates [width]
  (let [segment-length (dec width)
        max-value (/ segment-length 2)
        numbers (reverse (take
                          (* 4 (dec width))
                          (range (* width width) 0 -1)))
        xs (concat
            (repeat segment-length max-value)
            (range (dec max-value) (- (dec max-value) segment-length) -1)
            (repeat segment-length (* -1 max-value))
            (range (inc (* -1 max-value)) (inc max-value)))
        ys (concat
            (range (inc (* -1 max-value)) (inc max-value))
            (repeat segment-length max-value)
            (range (dec max-value) (- (dec max-value) segment-length) -1)
            (repeat segment-length (* -1 max-value)))
        coordinates (map vector xs ys)]
    (into {} (map vector numbers coordinates))))

(defn manhattan-distance [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn part-1 [input]
  (-> (border-width input)
      (width->coordinates)
      (get input)
      manhattan-distance))

(defn +-not-nil [ns]
  (->> (filter identity ns)
       (reduce +)))

(defn add-neighbours [grid [x y]]
  (+-not-nil
   [(grid [x (inc y)])
    (grid [(inc x) (inc y)])
    (grid [(inc x) y])
    (grid [(inc x) (dec y)])
    (grid [x (dec y)])
    (grid [(dec x) (dec y)])
    (grid [(dec x) y])
    (grid [(dec x) (inc y)])]))

(defn next-coord [border-width [x y]]
  (let [max-coord (/ (dec border-width) 2)
        min-coord (* -1 max-coord)]
    (cond
      (and (= x max-coord)
           (not= y max-coord))
      [x (inc y)]
      (and (= y max-coord)
           (not= x min-coord))
      [(dec x) y]
      (and (= x min-coord)
           (not= y min-coord))
      [x (dec y)]
      (and (= y min-coord)
           (not= x max-coord))
      [(inc x) y])))

(defn larger-than-target
  ([target] (larger-than-target {[0 0] 1} 3 [1 0] target))
  ([grid border-width current-coord target]
   (let [neighbour-sum (add-neighbours grid current-coord)
         next-grid (conj grid [current-coord neighbour-sum])
         squares (count next-grid)
         border-filled? (= squares (* border-width border-width))]
     (cond
       (> neighbour-sum target) neighbour-sum
       border-filled? (recur next-grid
                             (+ 2 border-width)
                             [(inc (first current-coord)) (last current-coord)]
                             target)
       :else (recur next-grid
                    border-width
                    (next-coord border-width current-coord)
                    target)))))

(defn part-2 [input]
  (larger-than-target input))

(comment
  (= 438 (part-1 input))
  (= 266330 (part-2 input)))
