(ns day2
  (:require [util]
            [clojure.string :as str]))

(defn parse [lines]
  (map (fn [line]
         (let [[dir n] (str/split line #"\ ")]
           [(keyword dir) (parse-long n)])) lines))

(defn move [instructions]
  (reduce (fn [[x y] [dir n]]
            (case dir
              :forward [(+ x n) y]
              :down [x (+ y n)]
              :up [x (- y n)]))
          [0 0]
          instructions))

(defn move2 [instructions]
  (take 2
        (reduce (fn [[x y aim] [dir n]]
                  (case dir
                    :forward [(+ x n) (+ y (* aim n)) aim]
                    :down [x y (+ aim n)]
                    :up [x y (- aim n)]))
                [0 0 0]
                instructions)))

(comment
  ;; part 1
  (apply * (move
            (parse (util/read-lines "example/day2.txt"))))

  (apply * (move
            (parse (util/read-lines "input/day2.txt"))))

  ;; part 2
  (apply * (move2
            (parse (util/read-lines "example/day2.txt"))))

  (apply * (move2
            (parse (util/read-lines "input/day2.txt"))))

  )
