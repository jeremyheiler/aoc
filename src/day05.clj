(require '[clojure.string :as str])

(def data
  (->> (slurp "src/day05.data")
       str/split-lines
       first))

(defn part1-step [polymer]
  (loop [ignored []
         [a b & x] polymer]
    (if b
      (if (or (= (int a) (+ (int b) 32))
              (= (+ (int a) 32) (int b)))
        (recur ignored x)
        (recur (conj ignored a) (cons b x)))
      (conj ignored a))))

(defn part1-react [polymer]
  (loop [polymer polymer]
    (let [new-polymer (part1-step polymer)]
      (if (= (count polymer) (count new-polymer))
        (apply str polymer)
        (recur new-polymer)))))

(defn part1-answer [polymer]
  (count (part1-react polymer)))

(comment
  (part1-react "dabAcCaCBAcCcaDA")
  (part1-answer "dabAcCaCBAcCcaDA")

  (part1-react data)
  (part1-answer data)
  )

(defn gen-alphabet []
  (apply str (map char (range (int \a) (inc (int \z))))))

(defn part2-step [polymer letter]
  (loop [ignored []
         [a & x] polymer]
    (if a
      (if (or (= a letter)
              (= (char (+ (int a) 32)) letter))
        (recur ignored x)
        (recur (conj ignored a) x))
      ignored)))

(defn part2-answer [polymer]
  (reduce (fn [shortest-length letter]
            (loop [polymer polymer]
              (let [new-polymer (part2-step polymer letter)]
                (if (= (count polymer) (count new-polymer))
                  (let [new-length (part1-answer polymer)]
                    (if (< new-length shortest-length)
                      new-length
                      shortest-length))
                  (recur new-polymer)))))
          (count polymer)
          (gen-alphabet)))

(comment
  (part2-answer "dabAcCaCBAcCcaDA")
  (part2-answer data)
  )
