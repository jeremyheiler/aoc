(require '[clojure.string :as str])

;; parse data

(def date-time-formatter
  (java.time.format.DateTimeFormatter/ofPattern "yyy-MM-dd HH:mm"))

(defn parse-line [line]
  (let [[_ timestamp action] (str/split line #"\[|(\] )")]
    {:date-time (java.time.LocalDateTime/parse timestamp date-time-formatter)
     :action action}))

(def data
  (->> (slurp "src/day04.data")
       str/split-lines
       (map parse-line)
       (sort-by :date-time)))

(def example-data
  (->> "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up"
       str/split-lines
       (map parse-line)
       (sort-by :date-time)))

(def conjv (fnil conj []))

(def nil+ (fnil + 0))

(defn diff-mins [date-time-early date-time-later]
  (.until date-time-early date-time-later java.time.temporal.ChronoUnit/MINUTES))

(defn part1-reduce [summary]
  (reduce (fn [acc [k v]]
            (conj acc (assoc v :guard-id k)))
          []
          summary))

(defn part1-summarize [events]
  (loop [[{:keys [date-time action] :as event} & events] events
         acc {}
         current-guard-id nil
         last-date-time nil
         total-min-asleep 0
         total-min-awake 0
         sleep-ranges []]
    ;;(println "--------------" sleep-ranges)
    (if event
      (if-let [[_ guard-id] (re-matches #"Guard #(\d+) begins shift" action)]
        (recur events
               (if current-guard-id
                 ;; finalize previous guard's records
                 (-> acc
                     (update-in [current-guard-id :total-min-asleep] nil+ total-min-asleep)
                     (update-in [current-guard-id :total-min-awake] nil+ total-min-awake)
                     (update-in [current-guard-id :sleep-ranges] concat sleep-ranges))
                 acc)
               (Long/parseLong guard-id)
               date-time
               0
               0
               [])
        (let [current-mins (diff-mins last-date-time date-time)]
          (cond
            (re-matches #"falls asleep" action)
            (do
              ;;(println current-guard-id "falls sleep")
              ;; save awake time
              (recur events
                     acc
                     current-guard-id
                     date-time
                     total-min-asleep
                     (+ total-min-awake current-mins)
                     sleep-ranges))
            (re-matches #"wakes up" action)
            (do
              ;;(println current-guard-id "wakes up")
              ;; save sleep time
              (recur events
                     acc
                     current-guard-id
                     date-time
                     (+ total-min-asleep current-mins)
                     total-min-awake
                     (let [minute-of-hour (.get last-date-time java.time.temporal.ChronoField/MINUTE_OF_HOUR)]
                       
                       ;;(println minute-of-hour)
                       ;;(println (+ minute-of-hour current-mins))
                       (conj sleep-ranges (range minute-of-hour (+ minute-of-hour current-mins)))))))))
      (-> acc
          (update-in [current-guard-id :total-min-asleep] nil+ total-min-asleep)
          (update-in [current-guard-id :total-min-awake] nil+ total-min-awake)
          (update-in [current-guard-id :sleep-ranges] concat sleep-ranges)
          part1-reduce))))

(defn part1-guard-with-most-min-asleep [summary]
  (reduce (fn [{:keys [guard-id total-min-asleep] :as m} event]
            (if (> (:total-min-asleep event) total-min-asleep)
              event
              m))
          summary))

(defn part1-most-common-min-asleep [{:keys [guard-id sleep-ranges]}]
  (clojure.pprint/pprint (->> sleep-ranges (apply concat) frequencies (sort-by second)))
  (conj (->> sleep-ranges (apply concat) frequencies (sort-by second) last vec) guard-id))

(defn part1-answer [{:keys [guard-id min-asleeps] :as guard}]
  (let [n (first (part1-most-common-min-asleep guard))]
    (println "============" n)
    (* n guard-id)))

(comment
  (clojure.pprint/pprint (part1-answer (part1-guard-with-most-min-asleep (part1-summarize data))))
  )

;; part 2

(defn part2-answer [summary]
  (let [[min freq guard-id] (->> summary
                                 (map part1-most-common-min-asleep)
                                 ;;(remove nil?)
                                 (sort-by second)
                                 last)]
    (* min guard-id)))

(comment
  (clojure.pprint/pprint (part2-answer (part1-summarize data)))
  (clojure.pprint/pprint (part1-summarize example-data))
  )
