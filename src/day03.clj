(require '[clojure.string :as str])

;; parse data

(defn parse-line [line]
  (let [[id _ xy wh] (str/split line #"\s")
        [x y] (str/split xy #",|:")
        [w h] (str/split wh #"x")]
    {:id (Long/parseLong (subs id 1))
     :x (Long/parseLong x)
     :y (Long/parseLong y)
     :w (Long/parseLong w)
     :h (Long/parseLong h)}))

(def data
  (->> (slurp "src/day3.data")
       str/split-lines
       (map parse-line)))

;; print claim diagrams

(defn print-claim [{:keys [id x y w h] :as claim}]
  (println "ID:" id)
  (println "X:" x)
  (println "Y:" y)
  (println "W:" w)
  (println "H:" h)
  (dotimes [i (+ y h 2)] ;; make the canvas 1 bigger than necessary
    (dotimes [j (+ x w 2)]
      (let [i (inc i)
            j (inc j)])
      (if (and (>= j x) (<= j (+ x w))
               (>= i y) (<= i (+ y h)))
        (print "#")
        (print ".")))
    (println)))

;; part 1

(def incnil (fnil inc 0))

(defn generate-xy-pairs [{:keys [x y w h]}]
  (for [i (range (inc y) (inc (+ y h)))
        j (range (inc x) (inc (+ x w)))]
    [i j]))

(defn generate-grid [claims]
  (loop [[{:keys [id x y w h] :as claim} & claims] claims
         grid {}]
    (if claim
      (let [xy-pairs (generate-xy-pairs claim)
            new-grid (reduce (fn [grid [i j]]
                               (update-in grid [i j] incnil))
                             grid
                             xy-pairs)]
        (recur claims new-grid))
      grid)))

(defn print-grid [grid]
  (dotimes [i 1000]
    (dotimes [j 1000]
      (let [i (inc i)
            j (inc j)]
        (if (>= (-> grid (get i 0) (get j 0)) 2) 
          (print "#")
          (print "."))))
    (println)))

(defn part1-answer [grid]
  (->> grid
       vals
       (map vals)
       (apply concat)
       (filter #(>= % 2))
       count))

;; part 2

(defn part2-answer [grid claims]
  (loop [[{:keys [id x y w h] :as claim} & claims] claims]
    (if claim
      (let [xy-pairs (generate-xy-pairs claim)]
        (if (reduce (fn [ok? [x y]]
                      (if (> (-> grid (get x) (get y)) 1)
                        (reduced false)
                        true))
                    grid
                    xy-pairs)
          id
          (recur claims)))
      :not-found)))
