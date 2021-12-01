(ns day1
  (:require [util]))

(defn count-inc [xs]
  (->> xs
       (partition 2 1)
       (map (partial apply <))
       (filter true?)
       count))

(comment
  ;; part 1
  (count-inc (util/read-longs "example/day1.txt"))

  (count-inc (util/read-longs "input/day1.txt"))

  )

(defn count-sliding-inc [xs]
  (->> xs
       (partition 3 1)
       (map (partial apply +))
       count-inc))

(comment
  ;; part 2
  (count-sliding-inc (util/read-longs "example/day1.txt"))

  (count-sliding-inc (util/read-longs "input/day1.txt"))

  )
