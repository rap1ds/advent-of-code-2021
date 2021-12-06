(ns day6
  (:require [util]
            [clojure.string :as str]))

(defn parse [lines]
  (map parse-long (str/split (first lines) #",")))

(defn day [freqs]
  (cond-> freqs
    (get freqs 0) (assoc 9 (get freqs 0))
    (get freqs 0) (update 7 (fnil + 0) (get freqs 0))
    true (dissoc 0)
    true (update-keys dec)))

(defn simulate-days [state days]
  (->> (frequencies state)
       (iterate day)
       (take (inc days))
       last
       vals
       (reduce +)))

(comment
  ;; part1
  (-> "example/day6.txt"
      util/read-lines
      parse
      (simulate-days 18)) ; #=> 26

  (-> "example/day6.txt"
      util/read-lines
      parse
      (simulate-days 80)) ; #=> 5934

  (time
   ;; "Elapsed time: 1.094828 msecs"
   (-> "input/day6.txt"
       util/read-lines
       parse
       (simulate-days 80)))

  ;; part2
  (-> "example/day6.txt"
      util/read-lines
      parse
      (simulate-days 256)) ; #=> 26984457539

  (time
   ;; "Elapsed time: 1.517747 msecs"
   (-> "input/day6.txt"
       util/read-lines
       parse
       (simulate-days 256)))
  )
