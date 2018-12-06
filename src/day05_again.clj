(require '[clojure.string :as str])

(def data
  (->> (slurp "src/day05.data")
       str/split-lines
       first))

(defn part1-collapse? [a b]
  (or (= (int a) (+ (int b) 32))
      (= (+ (int a) 32) (int b))))

(defn part1-react [polymer]
  (loop [ignored []
         last-unit nil
         [a & x] polymer]
    (if last-unit
      (if a
        (if (part1-collapse? last-unit a)
          (if (empty? ignored)
            (recur ignored nil x)
            (recur (pop ignored) (peek ignored) x))
          (recur (conj ignored last-unit) a x))
        (apply str (conj ignored last-unit)))
      (recur ignored a x))))

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

(defn part2-unit? [a unit]
  (or (= a unit)
      (= (char (+ (int a) 32)) unit)))

(defn part2-remove-unit [polymer unit]
  (reduce (fn [acc x]
            (if (part2-unit? x unit)
              acc
              (conj acc x)))
          []
          polymer))

(defn part2-answer [polymer]
  (reduce (fn [shortest-length unit]
            (let [new-polymer (part2-remove-unit polymer unit)]
              (let [new-length (part1-answer polymer)]
                (if (< new-length shortest-length)
                  new-length
                  shortest-length))))
          (count polymer)
          (gen-alphabet)))

(comment
  (part2-answer "dabAcCaCBAcCcaDA")
  (part2-answer data)
  )
