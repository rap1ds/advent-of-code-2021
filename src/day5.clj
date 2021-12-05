(ns day5
  (:require [util]
            [clojure.string :as str]))

(defn parse [lines]
  (->> lines
       (map #(let [[start end] (str/split % #" -> ")
                   [x1 y1] (str/split start #",")
                   [x2 y2] (str/split end #",")]
               [[(parse-long x1) (parse-long y1)]
                [(parse-long x2) (parse-long y2)]]))))

(defn horizontal? [[[_x1 y1] [_x2 y2]]]
  (= y1 y2))

(defn vertical? [[[x1 _y1] [x2 _y2]]]
  (= x1 x2))

(defn range* [start end]
  (cond
    (< start end)
    (range start (inc end))

    (= start end)
    (repeat start)

    :else
    (reverse (range end (inc start)))))

(defn expand [[[x1 y1] [x2 y2]]]
  (map (fn [x y] [x y]) (range* x1 x2) (range* y1 y2)))

(defn num-covered [expanded-lines]
  (->> expanded-lines
       (apply concat)
       frequencies))

(defn part1 [file]
  (->> file
       util/read-lines
       parse
       (filter #(or (horizontal? %)
                    (vertical? %)))
       (map expand)
       num-covered
       (filter #(<= 2 (val %)))
       count))

(defn part2 [file]
  (->> file
       util/read-lines
       parse
       (map expand)
       num-covered
       (filter #(<= 2 (val %)))
       count))

(comment
  ;; part 1
  (part1 "example/day5.txt") ; => 5

  ;; "Elapsed time: 99.940045 msecs"
  (time (part1 "input/day5.txt"))

  (part2 "example/day5.txt") ; => 12

  ;; "Elapsed time: 169.119134 msecs"
  (time (part2 "input/day5.txt"))
  )
