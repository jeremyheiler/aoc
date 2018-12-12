(require '[clojure.string :as str])

(defn parse-data [text]
  (->> text
       str/split-lines
       (map (fn [label xy]
              (let [[x y] (str/split xy #", ")]
                {:label label
                 :x (Long/parseLong x)
                 :y (Long/parseLong y)}))
            (map str
                 (map char (cycle (range (int \A) (inc (int \Z)))))
                 (cycle (apply concat (map #(repeat 26 %) (iterate inc 0))))))))

(def data
  (parse-data (slurp "src/day06.data")))

(def example-data
  (parse-data "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"))

(defn gen-table [coordinates size]
  (let [table (vec (repeat size (vec (repeat size ".." ;;(if (>= size 26) ".." ".")
                                             ))))]
    (reduce (fn [table {:keys [label x y]}]
              (assoc-in table [y x] "  " ;;label
                        ))
            table
            coordinates)))

(defn print-table [table]
  (->> table
       (map #(apply str %))
       (str/join "\n")
       println))

(comment
  (print-table (gen-table example-data 10))
  (print-table (gen-table data 360))
  )

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn xy-pair [{:keys [x y]}]
  [x y])

(defn find-closest-points [base-xy coordinates]
  ;; TODO: check for multiple closests?
  (reduce (fn [closest-points current-point]
            (if (= base-xy current-point)
              closest-points
              (if (empty? closest-points)
                [current-point]
                (let [current-point-distance (distance (xy-pair base-xy) (xy-pair current-point))
                      closest-point-distance (distance (xy-pair base-xy) (xy-pair (first closest-points)))]
                  (cond
                    (= current-point-distance closest-point-distance)
                    (conj closest-points current-point)

                    (< current-point-distance closest-point-distance)
                    [current-point]

                    :else
                    closest-points)))))
          []
          coordinates))

(defn infinite? [{:keys [x y]} size]
  (or (zero? x)
      (zero? y)
      (= x (dec size))
      (= y (dec size))))

(defn calculate-areas [coordinates size]
  (loop [[y & ys] (range size)
         table (gen-table coordinates size)
         areas {}]
    (if y
      (let [[table areas] (loop [[x & xs] (range size)
                                 table table
                                 areas areas]
                            (if x
                              (if (some #(= [x y] (xy-pair %)) coordinates)
                                (recur xs table areas)
                                (let [closest-points (find-closest-points {:x x :y y} coordinates)]
                                  (if (= (count closest-points) 1)
                                    (recur xs
                                           (assoc-in table [y x] (:label (first closest-points)))
                                           (-> areas
                                               (update-in [(:label (first closest-points)) :area] (fnil inc 0))
                                               (update-in [(:label (first closest-points)) :infinite?] #(or % (infinite? {:x x :y y} size)))))
                                    (recur xs table areas))))
                              [table areas]))]
        
        (recur ys table areas))
      [table areas])))

(defn part1-answer [[table areas]]
  (->> areas
       vals
       (remove :infinite?)
       (map :area)
       (apply max)))

(defn part1-table [[table areas]]
  (print-table table))

(comment
  (part1-answer (calculate-areas example-data 10))
  (part1-answer (calculate-areas data 360))
  )

(defn part2-answer [coordinates size max-region-size]
  (loop [[y & ys] (range size)
         region 0]
    (if y
      (recur ys (loop [[x & xs] (range size)
                       region region]
                  (if x
                    (if (< (apply + (map #(distance [x y] (xy-pair %)) coordinates)) max-region-size)
                      (recur xs (inc region))
                      (recur xs region))
                    region)))
      region)))

(comment
  (part2-answer example-data 10 32)
  (part2-answer data 360 10000)
  )
