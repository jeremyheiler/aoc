(require '[clojure.string :as str])

(def data
  (->> (slurp "src/day1.data")
      str/split-lines
      (map #(Long/parseLong %))))

;; part1
(reduce + 0 data)

;; part1: loop
(loop [[adj & adjs] data
       freq 0]
  (if adj
    (recur adjs (+ freq adj))
    freq))

;; part2
(loop [[adj & adjs] (apply concat (repeat data))
       iter 0
       freq 0
       seen #{}]
  (if (= 100000000 iter)
    :oops
    (let [new-freq (+ freq adj)]
      (if (contains? seen new-freq)
        new-freq
        (recur adjs (inc iter) new-freq (conj seen new-freq))))))
