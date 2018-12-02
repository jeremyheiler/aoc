(require '[clojure.string :as str])

(def data
  (->> (slurp "src/day2.data")
       str/split-lines))

;; part 1
(apply * (vals (reduce (fn [state id]
                         (let [freqs (into #{} (vals (frequencies (seq id))))]
                           (cond-> state
                             (contains? freqs 2) (update :total2 inc)
                             (contains? freqs 3) (update :total3 inc))))
                       {:total2 0
                        :total3 0}
                       data)))
;; part 2

;; this is exponential :-(

;; this assumes id1 and id2 are the same length
(defn differs-by-one? [id1 id2]
  (loop [[c1 & cs1] id1
         [c2 & cs2] id2
         has-one-diff? false]
    (if c1
      (if (= c1 c2)
        (recur cs1 cs2 has-one-diff?)
        (if has-one-diff?
          false
          (recur cs1 cs2 true)))
      has-one-diff?)))

(defn find-special-ids [data]
  (loop [[id1 & ids1] data]
    (if id1
      (if-let [result (loop [[id2 & ids2] data]
                        (when id2
                          (if (differs-by-one? id1 id2)
                            [id1 id2]
                            (recur ids2))))]
        result
        (recur ids1)))))

(defn remove-diff-char [id1 id2]
  (->> (map (fn [c1 c2]
              (when (= c1 c2)
                c1))
            (seq id1)
            (seq id2))
       (remove nil?)
       (apply str)))

(apply remove-diff-char (find-special-ids data))
