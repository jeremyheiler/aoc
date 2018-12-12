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
  (parse-data "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."))

(defn build-graph [edges]
  (reduce (fn [graph {:keys [depends-on step]}]
            (-> graph
                (update depends-on (fnil conj []) step)
                (update step #(or % []))))
	  {}
          edges))

(defn part1-answer [edges]
  (loop [unused-graph (build-graph edges)
         answer []]

    (println "-----------------")
    (println "UNUSED GRAPH" unused-graph)
    (println "ANSWER" answer)

    (if (empty? unused-graph)
      (apply str answer)
      (let [ready (sort
                   (remove
                    (into #{} (apply concat (vals unused-graph)))
                    (keys unused-graph)))
            picked (first ready)]

        (println "READY" ready)

        (recur (dissoc unused-graph picked)
               (conj answer picked))))))

(defn part2-handle-worker [base-seconds unused-graph [work-node seconds-remaining :as worker] in-progress]
  (if (nil? work-node)
    (if-let [first-ready (first
                          (drop-while in-progress
                                      (sort
                                       (remove
                                        (into #{} (apply concat (vals unused-graph)))
                                        (keys unused-graph)))))]
      [unused-graph
       [first-ready (+ base-seconds (- (int (first first-ready)) 64))]
       (conj in-progress first-ready)
       nil]
      [unused-graph
       [nil 0]
       in-progress
       nil])
    (if (zero? (dec seconds-remaining))
      (let [unused-graph (dissoc unused-graph work-node)]
        (if-let [first-ready (first
                              (drop-while in-progress
                                          (sort
                                           (remove
                                            (into #{} (apply concat (vals unused-graph)))
                                            (keys unused-graph)))))]
          [unused-graph
           [first-ready (+ base-seconds (- (int (first first-ready)) 64))]
           (-> in-progress
               (conj first-ready)
               (disj work-node))
           work-node]
          [unused-graph
           [nil 0]
           (disj in-progress work-node)
           work-node]))
      [unused-graph
       [work-node (dec seconds-remaining)]
       in-progress
       nil])))

(defn part2-answer [edges base-seconds num-workers]
  (loop [unused-graph (build-graph edges)
         in-progress #{}
         workers (map (constantly [nil 0]) (range num-workers))
         seconds -1
         answer []]

    (println "-----------------------------")
    (println "UNUSED GRAPH" unused-graph)
    (println "IN PROGRESS" in-progress)
    (println "WORKERS" workers)
    (println "SECONDS" seconds)
    (println "ANSWER" answer)
    
    (if (and (empty? unused-graph))
      [seconds (apply str answer)]
      (let [;;[unused-graph new-worker1 in-progress done1] (part2-handle-worker base-seconds unused-graph worker1 in-progress)
            ;;[unused-graph new-worker2 in-progress done2] (part2-handle-worker base-seconds unused-graph worker2 in-progress)
            ;;[unused-graph new-worker3 in-progress done3] (part2-handle-worker base-seconds unused-graph worker3 in-progress)
            ;;[unused-graph new-worker4 in-progress done4] (part2-handle-worker base-seconds unused-graph worker4 in-progress)
            ;;[unused-graph new-worker5 in-progress done5] (part2-handle-worker base-seconds unused-graph worker5 in-progress)

            [unused-graph new-workers in-progress done]
            (loop [unused-graph unused-graph
                   [w & ws] (concat (remove #(nil? (first %)) workers)
                                    (filter #(nil? (first %)) workers))
                   new-workers []
                   in-progress in-progress
                   done []]
              (if w
                (let [[unused-graph new-w in-progress done-val] (part2-handle-worker base-seconds unused-graph w in-progress)]
                  (recur unused-graph ws (conj new-workers new-w) in-progress (conj done done-val)))
                [unused-graph new-workers in-progress done]))
            
            ]
        (recur unused-graph
               in-progress
               new-workers
               (inc seconds)
               (apply conj answer (remove nil? done)))))))
