(require '[clojure.string :as str])

(defn parse-data [text]
  (->> text
       str/split-lines
       (map #(re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin\." %))
       (map (fn [[_ depends-on step]]
              {:depends-on depends-on
               :step step}))))

(def data
  (parse-data (slurp "src/day07.data")))

(def example-data
  (parse-data (slurp "src/day07.example-data")))

(def conjv (fnil conj []))

(defn build-deps [data]
  (reduce (fn [deps {:keys [depends-on step]}]
            (-> deps
                (update depends-on conjv step)
                (update step #(or % []))))
	  {}
          data))

(defn shrink-deps [deps]
  (remove
   (into #{} (apply concat (vals deps)))
   (keys deps)))

(defn part1-answer [data]
  (loop [unused-deps (build-deps data)
         answer []]
    (if (empty? unused-deps)
      (apply str answer)
      (let [selected (first (sort (shrink-deps unused-deps)))]
        (recur (dissoc unused-deps selected)
               (conj answer selected))))))

(defn part2-handle-worker [base-seconds unused-deps in-progress [node remaining-seconds]]
  (if (nil? node)
    (if-let [first-ready (first (drop-while in-progress (sort (shrink-deps unused-deps))))]
      [unused-deps
       (assoc in-progress first-ready (+ base-seconds (- (int (first first-ready)) 64)))
       nil]
      [unused-deps
       in-progress
       nil])
    (if (zero? (dec remaining-seconds))
      (let [unused-deps (dissoc unused-deps node)]
        (if-let [first-ready (first (drop-while in-progress (sort (shrink-deps unused-deps))))]
          [unused-deps
           (-> in-progress
               (assoc first-ready (+ base-seconds (- (int (first first-ready)) 64)))
               (dissoc node))
           node]
          [unused-deps
           (dissoc in-progress node)
           node]))
      [unused-deps
       (update in-progress node dec)
       nil])))

(defn part2-answer [data base-seconds num-workers]
  (loop [unused-deps (build-deps data)
         in-progress {}
         seconds -1
         answer []]
    (if (and (empty? unused-deps))
      [seconds (apply str answer)]
      (let [[unused-deps in-progress done] (loop [unused-deps unused-deps
                                                  in-progress in-progress
                                                  [w & ws] (take num-workers (concat
                                                                              (remove #(nil? (key %)) in-progress)
                                                                              (cycle [[nil 0]])))
                                                  done []]
                                             (if w
                                               (let [[unused-deps in-progress done-val] (part2-handle-worker base-seconds unused-deps in-progress w)]
                                                 (recur unused-deps in-progress ws (conj done done-val)))
                                               [unused-deps in-progress done]))]
        (recur unused-deps
               in-progress
               (inc seconds)
               (apply conj answer (remove nil? done)))))))
