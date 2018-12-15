(require '[clojure.string :as str])

(defn parse-data [text]
  (map #(Long/parseLong %) (str/split text #"[ \n]")))

(def data
  (parse-data (slurp "src/day08.data")))

(def example-data
  (parse-data "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"))

(defn part1-answer [data]
  (loop [data data
         processing-stack []
         metadata-sum 0]
    (if (empty? data)
      metadata-sum
      (if (empty? processing-stack)
        ;; get another node to process
        (let [[num-children num-metadata & data] data]
          (recur data
                 (conj processing-stack [num-children num-metadata])
                 metadata-sum))
        (let [[current-num-children current-num-metadata] (peek processing-stack)]
          (if (zero? current-num-children)
            ;; no more children to process, so sum metadata
            (recur (drop current-num-metadata data)
                   (pop processing-stack)
                   (apply + metadata-sum (take current-num-metadata data)))
            ;; more children to process
            (let [[num-children num-metadata & data] data]
              (recur data
                     (conj (pop processing-stack) [(dec current-num-children) current-num-metadata] [num-children num-metadata])
                     metadata-sum))))))))

(declare build-children)

(defn build-node [data]
  (when (seq data)
    (let [[num-children num-metadata & rest-data] data
          [new-data children] (build-children num-children rest-data)
          new-new-data (drop num-metadata new-data)
          node [children (vec (take num-metadata new-data))]]
      [new-new-data node])))

(defn build-children [num-children data]
  (loop [data data
         children []]
    (if (empty? data)
      [data children]
      (if (= num-children (count children))
        [data children]
        (let [[new-data node] (build-node data)]
          (recur new-data (conj children node)))))))

(defn build-tree [data]
  (let [[new-data node] (build-node data)]
    node))

(defn part2-answer [tree]
  (let [[children metadata] tree]
    (if (seq children)
      (let [child-values (map part2-answer children)]
        (->> metadata
             (map #(nth child-values (dec %) 0))
             (apply +)))
      (apply + metadata))))
